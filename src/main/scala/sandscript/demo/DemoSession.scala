/**
  * Copyright 2021. All rights reserved.
  * Author: Xinyan Lu (luxinyan@outlook.com)
  */
package sandscript.demo

import sandscript.{ValueChecker, SandSession}
import sandscript.SandElement.Markdown

import scala.util.Random

class DemoSession extends SandSession {
  override val title = "猜数字"
  override val description = ""

  override def process(): Unit = {
    show(Markdown(raw"""
      |## $title
      |> Author：Xinyan Lu (luxinyan@outlook.com)
      |> Version: 2021.02.16
      |
      |数字范围是 0 到 9，如果 ~~猜错~~ 会有提示。
      """.stripMargin))

    val rand = Random.nextInt(10)
    logToClient("The number is: " + rand)

    var input = -1
    while (input != rand) {
      input = inputSimple("请输入一个整数：", ValueChecker.IsInteger).toInt
      if (input > rand) {
        logToClient(input + " is too large")
      } else if (input < rand) {
        logToClient(input + " is too small")
      } else {
        show(Markdown("### Bingo" + "\n 正确数字是：" + input))
      }
    }
  }

}
