/**
  * Copyright 2021. All rights reserved.
  * Author: Xinyan Lu (luxinyan@outlook.com)
  */
package sandscript

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.{LinkedBlockingDeque, TimeUnit}

import fi.iki.elonen.NanoWSD.WebSocket
import fi.iki.elonen.NanoWSD.WebSocketFrame.CloseCode
import org.slf4j.LoggerFactory
import sandscript.SandElement._
import sandscript.ValueChecker.Checker
import scalatags.Text.TypedTag
import scalatags.Text.all._

import scala.collection.mutable

// TODO: 增加HelpSession
abstract class SandSession {
  implicit val thisSession: SandSession = this

  // 预留 100 之前的用于特殊用途
  private val nextId = new AtomicLong(100L)
  private val nodes: mutable.Map[Long, Node] = mutable.Map[Long, Node]()

  protected val logger = LoggerFactory.getLogger(this.getClass)
  val title: String
  val description: String

  protected def process(): Unit

  private var webSocket: WebSocket = _

  def setWebSocket(webSocket: WebSocket): SandSession = {
    if (this.webSocket == null) {
      this.webSocket = webSocket
      this
    } else {
      throw new Exception("Init WebSocket twice.")
    }
  }

  private def send(command: Command): Unit = {
    webSocket.send(command.toString)
  }

  case class Node(id: Long) {

    /** 主要用于异步获取客户端传来的输入信息 */
    val msgQueue = new LinkedBlockingDeque[Map[String, String]]()

    def delete(): Unit = {
      send(Command(CommandOp.Delete, id))
      nodes.remove(id)
    }

    def append(data: (String, String)*): Node = {
      send(Command(CommandOp.Append, id, data.toMap))
      this
    }

    def set(data: (String, String)*): Node = {
      send(Command(CommandOp.Set, id, data.toMap))
      this
    }

    def get(timeoutInSec: Long = 600): Map[String, String] = {
      msgQueue.clear()
      val ret = msgQueue.poll(timeoutInSec, TimeUnit.SECONDS)
      if (ret == null) {
        throw new RuntimeException(
          "User input timeout: %d seconds".format(timeoutInSec)
        )
      }
      ret
    }
  }

  /** 创建新的节点 */
  final def newNode(): Node = {
    val nodeId = nextId.getAndIncrement()
    val node = Node(nodeId)
    nodes.update(nodeId, node)
    node
  }

  /** 结束Session */
  final def close(): Unit = {
    logToClient("Server disconnected.")
    send(Command(CommandOp.Quit, -1, Map("reason" -> "")))
    Thread.sleep(1000)
    webSocket.close(CloseCode.NormalClosure, "", false)
  }

  final def onMessage(line: String): Unit = {
    Command.fromString(line) match {
      case Some(command) if command.command == CommandOp.Set =>
        val nodeId = command.nodeId
        nodes.get(nodeId).foreach { node =>
          node.msgQueue.offer(command.data)
        }
      case _ => // do nothing
    }
  }

  final def onClose(reason: String): Unit = {
    logger.info("Close reason: " + reason)
  }

  final def onOpen(): Unit = {
    val thread = new Thread {
      override def run(): Unit = {
        try {
          setPageTitle(title)
          logToClient("Connected to server successfully.")
          process()
          logToClient("Task completed.")
          close()
        } catch {
          case exception: Exception =>
            val msg = "Interrupted with exception: " + exception.getMessage
            logger.warn(msg)
            if (webSocket.isOpen) {
              logToClient(msg)
              close()
            }
        }
      }
    }
    thread.start()
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////

  /** 输出日志提示到客户端 */
  final def logToClient(message: String): Unit = {
    val timestamp = LocalDateTime
      .now()
      .format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS"))
    val fullMessage = "+ " + timestamp + " - " + message
    send(Command(CommandOp.Log, -1, Map("message" -> fullMessage)))
  }

  /** 设置页面标题 */
  final def setPageTitle(str: String): Unit = {
    send(Command(CommandOp.Attr, -1, Map("title" -> str)))
  }

  /** 显示内容到新节点。如果expiredInSec为正数，则在expiredInSec秒后自动销毁 */
  def show(content: TypedTag[String], expiredInSec: Int = -1): Node = {
    val node = newNode()
    if (expiredInSec <= 0) {
      node.set("html" -> content.render)
    } else {
      node.set("html" -> content.render, "expired" -> expiredInSec.toString)
    }
  }

  /** 获取组合输入的信息 */
  def inputGroup(elements: InputElement*)(implicit ss: SandSession): Map[String, String] = {
    var inputs: Map[String, String] = null
    var errorMessage = ""

    def checkInputs: Boolean =
      elements.forall { ele =>
        val value = inputs.getOrElse(ele.key, "")
        val checkResult = ele.checker.check(value)
        if (!checkResult) {
          val msg = ele.checker.errorText + ": " + value
          errorMessage = msg
        }
        checkResult
      }

    val node = ss.newNode()

    do {
      val html = Notification(
        Form(
          elements.map(_.show),
          if (errorMessage.nonEmpty) Message(errorMessage).isDanger else Div,
          Buttons(SubmitButton, ResetButton)
        )
      )
      inputs = node.set("html" -> html.render).get()
    } while (!checkInputs)
    node.delete()
    inputs
  }

  /** 获取单个输入的信息 */
  def inputSimple(prompt: String, checker: Checker): String = {
    val key = "input"
    inputGroup(inputLine(key, prompt, checker)).getOrElse(key, "")
  }
}
