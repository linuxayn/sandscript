/**
  * Copyright 2021. All rights reserved.
  * Author: Xinyan Lu (luxinyan@outlook.com)
  */
package sandscript

import java.io.{File, FileInputStream, IOException}

import fi.iki.elonen.NanoHTTPD.IHTTPSession
import fi.iki.elonen.NanoWSD.{WebSocket, WebSocketFrame}
import fi.iki.elonen.{NanoHTTPD, NanoWSD}
import org.slf4j.LoggerFactory

import scala.util.Try

class SandServer(host: String, port: Int, route: Map[String, Class[_ <: SandSession]])
    extends NanoWSD(host, port) {
  private val logger = LoggerFactory.getLogger(getClass)

  override def start(): Unit = {
    super.start(0, false)
  }

  private def serveFile(session: IHTTPSession): NanoHTTPD.Response = {
    val uri = session.getUri.substring(1) match {
      case u if route.contains(u) => "index.html"
      case u                      => u
    }
    logger.info("Request URL: " + uri)
    val fileExt = Try(uri.substring(uri.lastIndexOf(".") + 1)).getOrElse("")
    val mime = fileExt match {
      case "ico"  => "image/x-icon"
      case "css"  => "text/css"
      case "htm"  => "text/htm"
      case "html" => "text/html"
      case "js"   => "application/javascript"
      case _      => "text/plain"
    }
    val inputStream = getClass.getResourceAsStream("/static" + "/" + uri)
    if (inputStream != null) {
      NanoHTTPD.newChunkedResponse(
        NanoHTTPD.Response.Status.OK,
        mime,
        inputStream
      )
    } else {
      // search in a local directory for debugging static page?
      val file = new File("d:/vue/" + uri)
      if (file.exists()) {
        NanoHTTPD.newChunkedResponse(NanoHTTPD.Response.Status.OK, mime, new FileInputStream(file))
      } else {
        NanoHTTPD.newFixedLengthResponse("Not Found")
      }
    }
  }

  override def serveHttp(session: IHTTPSession): NanoHTTPD.Response = serveFile(session)

  override def openWebSocket(httpSession: NanoHTTPD.IHTTPSession): NanoWSD.WebSocket = {
    val uri = httpSession.getUri.substring(1)
    logger.info("WebSocket Uri: " + uri)
    if (route.contains(uri)) {
      val sandSession = route(uri).newInstance()
      val sandSocket = new SandSocket(httpSession, sandSession)
      sandSession.setWebSocket(sandSocket)
      sandSocket
    } else {
      // do nothing
      logger.warn("Found no route.")
      new EmptySocket(httpSession)
    }
  }

  class EmptySocket(httpSession: IHTTPSession) extends WebSocket(httpSession) {
    override def onOpen(): Unit = {
      close(WebSocketFrame.CloseCode.GoingAway, "Empty", false)
    }

    override def onClose(code: WebSocketFrame.CloseCode,
                         reason: String,
                         initiatedByRemote: Boolean): Unit = {}

    override def onMessage(message: WebSocketFrame): Unit = {}

    override def onPong(pong: WebSocketFrame): Unit = {}

    override def onException(exception: IOException): Unit = {}
  }

  class SandSocket(httpSession: IHTTPSession, sandSession: SandSession)
      extends WebSocket(httpSession) {

    override def onOpen(): Unit = {
      logger.info("Client open: " + httpSession.getRemoteIpAddress)
      sandSession.onOpen()
    }

    override def onClose(code: WebSocketFrame.CloseCode,
                         reason: String,
                         initiatedByRemote: Boolean): Unit = {
      logger.info("Client close: " + httpSession.getRemoteIpAddress)
      sandSession.onClose(reason)
    }

    override def onMessage(message: NanoWSD.WebSocketFrame): Unit = {
      sandSession.onMessage(message.getTextPayload)
    }

    override def onPong(pong: NanoWSD.WebSocketFrame): Unit = {
      logger.info("Received pong: " + httpSession.getRemoteIpAddress)
    }

    override def onException(exception: IOException): Unit = {
      logger.warn(
        "Exception from %s: %s"
          .format(httpSession.getRemoteIpAddress, exception.getMessage)
      )
    }
  }
}
