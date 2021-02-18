/**
  * Copyright 2021. All rights reserved.
  * Author: Xinyan Lu (luxinyan@outlook.com)
  */
package sandscript

import scala.util.Try

object ValueChecker {

  trait Checker {
    def check(str: String): Boolean

    /** 检查不通过时的出错信息 */
    def errorText: String
  }

  /** 不进行任何检查 */
  object NoCheck extends Checker {
    override def check(str: String) = true
    override def errorText = ""
  }

  /** 是否非空 */
  object NonEmpty extends Checker {
    override def check(str: String) = str.nonEmpty
    override def errorText = "should not be empty"
  }

  /** 是否是全字母 */
  object IsDigit extends Checker {
    override def check(str: String) = str.forall(_.isDigit)
    override def errorText = "should be all digits"
  }

  /** 是否是整型 */
  object IsInteger extends Checker {
    override def check(str: String) = Try(str.toLong).isSuccess
    override def errorText = "should be an integer"
  }

  /** 是否是浮点型 */
  object IsFloat extends Checker {
    override def check(str: String) = Try(str.toFloat).isSuccess
    override def errorText = "should be a float"
  }

  /** 是否是ones的其中一个 */
  def IsOneOf(ones: Set[String]) = new Checker {
    override def check(str: String) = ones.contains(str)
    override def errorText =
      "should be one of " + ones.map(one => "<" + one + ">").mkString(", ")
  }
}
