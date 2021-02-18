/**
  * Copyright 2021. All rights reserved.
  * Author: Xinyan Lu (luxinyan@outlook.com)
  */
package sandscript.demo

import sandscript.SandServer

object Main {
  def main(args: Array[String]): Unit = {
    val route = Map("demo" -> classOf[DemoSession])
    val server = new SandServer("localhost", 8080, route)
    server.start()
  }
}
