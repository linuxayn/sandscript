/**
  * Copyright 2021. All rights reserved.
  * Author: Xinyan Lu (xinyanlu@tencent.com)
  */
package sandscript

import java.util.Base64

// 服务端-客户端交互指令
object CommandOp extends Enumeration {
  type CommandOp = Value
  // 设置（覆盖）内容
  val Set: Value = Value("set")
  // 追加内容
  val Append: Value = Value("append")
  // 销毁
  val Delete: Value = Value("delete")

  // 退出Session
  val Quit: Value = Value("quit")
  // Log到客户端
  val Log: Value = Value("log")
  // 设置属性
  val Attr: Value = Value("attr")
}

// 服务端-客户端交互命令，传输为纯文本，格式为:
// 指令类型 \t 节点Id \t 参数键1 encode(参数值1) 参数键2 encode(参数值2) ...
case class Command(command: CommandOp.Value, nodeId: Long, data: Map[String, String] = Map.empty) {
  private def encode(data: String): String =
    Base64.getEncoder.encodeToString(data.getBytes("utf-8"))

  override def toString = {
    val dataStr =
      data.map(item => item._1 + " " + encode(item._2)).mkString(" ")
    command.toString + "\t" + nodeId + "\t" + dataStr
  }
}

object Command {
  private def decode(encoded: String): String =
    new String(Base64.getDecoder.decode(encoded))

  def fromString(line: String): Option[Command] = {
    val parts = line.split("\t")
    if (parts.length < 2) {
      None
    } else {
      val data = if (parts.length == 3) {
        val tokens = parts(2).split(" ")
        tokens
          .grouped(2)
          .map { pair =>
            pair(0) -> decode(pair(1))
          }
          .toMap
      } else {
        Map[String, String]()
      }
      Some(Command(CommandOp.withName(parts(0)), parts(1).toLong, data))
    }
  }
}
