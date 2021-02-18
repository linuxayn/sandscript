/**
  * Copyright 2021. All rights reserved.
  * Author: Xinyan Lu (luxinyan@outlook.com)
  */
package sandscript

import org.pegdown.PegDownProcessor
import sandscript.ValueChecker.Checker
import scalatags.Text.TypedTag

/**
  * 可展示的 HTML 元素。
  * 元素命名惯例：以大写字母开头的
  */
object SandElement {

  import scalatags.Text.all._

  implicit class TypedTagHelper[T <: String](typedTag: TypedTag[T]) {
    def add(str: String): TypedTag[T] = typedTag.apply(cls := " " + str)

    // format:off
    // style
    def isNormal = add("is-normal")
    def isStatic = add("is-static")
    def isPrimary = add("is-primary")
    def isLink = add("is-link")
    def isInfo = add("is-info")
    def isSuccess = add("is-success")
    def isDanger = add("is-danger")
    def isWarning = add("is-warning")
    def isLight = add("is-light")
    def isDark = add("is-dark")
    def isRounded = add("is-rounded")
    def isText = add("is-text")
    def isWhite = add("is-white")
    def isSmall = add("is-small")
    def isLarge = add("is-large")
    def isCentered = add("is-centered")
    def isRight = add("is-right")
    def isGrouped = add("is-grouped")

    def addTooltip(tip: String) =
      typedTag(attr("data-tooltip") := tip)
        .add("has-tooltip-arrow has-tooltip-multiline")
    // format: on
  }

  lazy val Div = div()
  lazy val ContentDiv = div(cls := "content")
  lazy val FieldDiv = div(cls := "field")
  lazy val ControlDiv = div(cls := "control")
  lazy val Form = form()
  lazy val Label = label(cls := "label")
  lazy val Button = button(cls := "button")
  lazy val SubmitButton = Button(`type` := "submit")("提交").isLink
  lazy val ResetButton = Button(`type` := "reset")("重置")
  lazy val Input = input(cls := "input")
  lazy val TextInput = Input(`type` := "text")
  lazy val PasswordInput = Input(`type` := "password")
  lazy val TextArea = textarea(cls := "textarea")

  lazy val Notification = div(cls := "notification")
  lazy val ProgressBar =
    tag("progress")(cls := "progress is-success", attr("max") := 100)
  lazy val Box = div(cls := "box")
  lazy val DeleteButton = button(cls := "delete")

  lazy val Title = h3(cls := "title is-4")
  lazy val SubTitle = h5(cls := "subtitle is-6")
  lazy val Tag = span(cls := "tag")

  /** 组合按钮 */
  def Buttons(buttons: TypedTag[String]*): TypedTag[String] = {
    val newTags = buttons.map { b =>
      ControlDiv(b)
    }
    FieldDiv(newTags).isGrouped
  }

  /** 构建信息 */
  def Message(msg: String, title: Option[String] = None): TypedTag[String] = {
    tag("article")(cls := "message")(
      if (title.nonEmpty) {
        div(cls := "message-header")(title.get)
      } else {
        Div
      },
      div(cls := "message-body py-3")(msg)
    )
  }

  /** 构建表格 */
  def Table(headers: Seq[String], rows: Seq[Seq[String]]): TypedTag[String] = {
    table(
      th(headers.map(d => td(d))),
      rows.map(row => tr(row.map(d => td(d))))
    )
  }

  /** 使用 Markdown 文本构建 */
  def Markdown(mdText: String): TypedTag[String] = {
    import org.pegdown.Extensions._
    val html = new PegDownProcessor(ALL).markdownToHtml(mdText)
    tag("article")(cls := "markdown-body")(raw(html))
  }

  /**
    * 输入元素
    * @param key 输入键，后续可在 Map 中获取对应的输入值
    * @param show 页面展现的内容（例如输入框）
    * @param checker 对输入值的检查器
    */
  case class InputElement(key: String, show: TypedTag[String], checker: Checker)

  /**
    * 单行文本输入框
    * @param key 变量名
    * @param prompt 提示语
    * @param checker 检查器
    */
  def inputLine(key: String, prompt: String, checker: Checker): InputElement = {
    val tag = FieldDiv(Label(prompt), ControlDiv(TextInput(name := key)))
    InputElement(key, tag, checker)
  }

  /**
    * 多行文本输入框
    * @param key 变量名
    * @param prompt 提示语
    * @param showRows 显示的行数
    * @param checker 检查器
    */
  def inputMultiLine(key: String, prompt: String, showRows: Int, checker: Checker): InputElement = {
    val tag = FieldDiv(
      Label(prompt),
      ControlDiv(TextArea(name := key, rows := showRows))
    )
    InputElement(key, tag, checker)
  }

  /** 单选下拉框，options：值 -> 提示语 */
  def inputSelect(key: String, options: (String, String)*) = {
    val newOptions = options.map(kv => option(value := kv._1)(kv._2))
    // format: off
    div(cls := "select")(
      select(name := key)(newOptions)
    )
    // format: on
  }

  /**
    * 多选输入框
    * @param showRows 显示多少行
    * @param options 值 -> 提示语
    */
  def inputMultiSelect(key: String, showRows: Int, options: (String, String)*): InputElement = {
    val values = options.map(_._1).toSet
    assert(options.length == values.size, "Duplicate options")
    val tag = div(cls := "select is-multiple")(
      select(name := key, multiple, size := showRows) {
        options.map {
          case (_value, prompt) => option(value := _value)(prompt)
        }
      }
    )
    InputElement(key, tag, ValueChecker.IsOneOf(values))
  }

  /** 复选框 */
  def inputCheckBox(key: String, prompt: String): InputElement = {
    // format: off
    val tag = label(cls := "checkbox")(
      input(`type` := "checkbox", name := key, value := key),
      prompt
    )
    // format: on
    InputElement(key, tag, ValueChecker.IsOneOf(Set(key)))
  }

  /** 单选框，options：值 -> 提示语 */
  def inputRadio(key: String, options: (String, String)*): InputElement = {
    val values = options.map(_._1).toSet
    assert(values.size == options.length, "Duplicate options")

    // format: off
    val newOptions = options.map{ case (_value, prompt) =>
      label(cls := "radio")(
        input(`type` := "radio", name := key, value := _value),
        prompt
      )
    }
    val tag = ControlDiv(newOptions)
    // format: off
    InputElement(key, tag, ValueChecker.IsOneOf(values))
  }

  // todo: 实现 websocket 文件上传（sandbox）



}
