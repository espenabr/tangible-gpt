package xf.interactionhandlers

import xf.model.InteractionHandler
import xf.model.Table.Column
import xf.model.Table
import xf.model.Table.Column.{BooleanColumn, NumberColumn, SingleChoiceColumn, TextColumn}
import cats.implicits.*
import xf.parsers.Parsers

object ResponseAsTable:

  case class TabularDataRequest(
      prompt: String,
      columns: List[Column]
  )

  val tableHandler = new InteractionHandler[TabularDataRequest, Table](
    _.prompt,
    r => specifyTableFormat(r.columns),
    (r, s) => Parsers.parseTable(r.columns)(s)
  )

  def specifyTableFormat(columns: List[Column]): String =
    def describeColumn(c: Column) = c match
      case BooleanColumn(name)               => s"$name: Boolean (true or false)"
      case NumberColumn(name)                => s"$name: Number (any number including decimal)"
      case TextColumn(name)                  => s"$name: String"
      case SingleChoiceColumn(name, options) => s"$name: One of the following values: ${options.mkString(", ")}"

    def example(c: Column) = c match
      case BooleanColumn(_)               => "false"
      case NumberColumn(_)                => "4"
      case TextColumn(_)                  => "blah blah"
      case SingleChoiceColumn(_, options) => options.head

    s"""I need a table in csv format, using ; (semicolon) as a separator. One row per line.
       |
       |${columns.map(describeColumn).mkString("\n")}
       |
       |Example:
       |${columns.map(example).mkString(";")}
       |
       |I do not want any header row, and I want nothing else in the response except the table rows.""".stripMargin
