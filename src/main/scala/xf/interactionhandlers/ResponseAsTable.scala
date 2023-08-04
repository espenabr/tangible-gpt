package xf.interactionhandlers

import xf.model.InteractionHandler
import xf.model.Table.Column
import xf.model.Table
import xf.model.Table.Row
import xf.model.Table.Column.{BooleanColumn, NumberColumn, SingleChoiceColumn, TextColumn}
import xf.model.Table.Cell.{BooleanCell, NumberCell, SingleChoiceCell, TextCell}
import cats.implicits.*

object ResponseAsTable {

  case class TabularDataRequest(
      prompt: String,
      columns: List[Column]
  )

  val tableHandler = new InteractionHandler[TabularDataRequest, Table](
    "",
    _.prompt,
    r => specifyTableFormat(r.columns),
    (r, s) => parseTable(s, r.columns)
  )

  private def specifyTableFormat(columns: List[Column]): String = {
    def describeColumn(c: Column) = c match {
      case BooleanColumn(name)               => s"$name: Boolean (true or false)"
      case NumberColumn(name)                => s"$name: Number (any number including decimal)"
      case TextColumn(name)                  => s"$name: String"
      case SingleChoiceColumn(name, options) => s"$name: One of the following values: ${options.mkString(", ")}"
    }

    def example(c: Column) = c match {
      case BooleanColumn(_)               => "false"
      case NumberColumn(_)                => "4216"
      case TextColumn(_)                  => "Gordon Blue"
      case SingleChoiceColumn(_, options) => options.head
    }

    s"""I need a table in csv format, using ; (semicolon as a separator. One row per line.
       |
       |${columns.map(describeColumn).mkString("\n")}
       |
       |Example:
       |${columns.map(example).mkString(";")}
       |
       |I do not want any header row, and I want nothing else in the response except the table rows.""".stripMargin
  }
}

private def parseTable(s: String, columns: List[Column]): Option[Table] = {
  val lines = s.split("\n").toList.filter(_.contains(";"))
  val rows  = lines.map { line =>
    val parts = line.split(";")
    val cells = columns.indices.zip(columns).toList.map { case (index, column) =>
      val part: String = parts(index)
      column match
        case BooleanColumn(_)               => parseBoolean(part).map(p => BooleanCell(p, column))
        case NumberColumn(_)                => part.toDoubleOption.map(n => NumberCell(n, column))
        case TextColumn(_)                  => Some(TextCell(part, column))
        case SingleChoiceColumn(_, options) =>
          parseSingleChoice(part, options).map(s => SingleChoiceCell(s, column))
    }
    if cells.forall(_.isDefined) then Some(Row(cells.flatMap(_.toList))) else None
  }

  if rows.forall(_.isDefined) then Some(Table(columns, rows.flatMap(_.toList))) else None
}

private def parseBoolean(s: String): Option[Boolean] = s.toLowerCase match {
  case "false" | "no" | "n" | "0" => Some(false)
  case "true" | "yes" | "y" | "1" => Some(true)
  case _                          => None
}

private def parseSingleChoice(s: String, options: List[String]): Option[String] = options.find(_ === s)
