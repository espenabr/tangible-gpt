package xf

import xf.CustomTypes.SingleChoice

import scala.util.Try
import xf.model.ResponseHandler
import cats.implicits.*
import xf.model.Table
import xf.model.Table.{Cell, Column, Row}
import xf.model.Table.Column.{BooleanColumn, NumberColumn, SingleChoiceColumn, TextColumn}
import xf.model.Table.Cell.{BooleanCell, NumberCell, SingleChoiceCell, TextCell}

object ResponseHandlers {

  /** Response handlers are responsible for prompting GPT for a specific type as a response and also parsing the
    * response. These can be created for a specific use case, but here we provide some handlers for common types.
    */

  val stringResponseHandler = new ResponseHandler[String](
    s => Some(s),
    ""
  )

  val integerResponseHandler = new ResponseHandler[Int](
    _.toIntOption,
    "I only want a single integer. Nothing else."
  )

  val doubleResponseHandler = new ResponseHandler[Double](
    _.toDoubleOption,
    "I only want a single number (with or without decimals). Nothing else."
  )

  val percentageResponseHandler = new ResponseHandler[Double](
    _.toDoubleOption.filter(d => d >= 0 && d <= 100),
    "I only want a percentage. Nothing else."
  )

  val booleanResponseHandler = new ResponseHandler[Boolean](
    parseBoolean,
    """I only want a single "yes" or "no" answer. Nothing else."""
  )

  def listResponseHandler(noOfOptions: Option[Int]) = new ResponseHandler[List[String]](
    s => Some(s.split("\n").toList),
    specifyListFormat(noOfOptions)
  )

  def singleChoiceResponseHandler(options: List[String]) = new ResponseHandler[SingleChoice](
    s => options.find(_ === s).map(c => SingleChoice(options, c)),
    s"""I only want the response to be one of the following values: ${options.mkString(", ")}
       |Nothing else.""".stripMargin
  )

  def tableResponseHandler(columns: List[Column]) = new ResponseHandler[Table](
    s => parseTable(s, columns),
    specifyTableFormat(columns)
  )

  // parsers

  private def parseBoolean(s: String): Option[Boolean] = s.toLowerCase match {
    case "false" | "no" | "n" | "0" => Some(false)
    case "true" | "yes" | "y" | "1" => Some(true)
    case _                          => None
  }

  private def parseTable(message: String, columns: List[Column]): Option[Table] = {
    val lines = message.split("\n").toList.filter(_.contains(";"))
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

  private def parseSingleChoice(s: String, options: List[String]): Option[String] = options.find(_ === s)

  // prompting for specific formats

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

  private def specifyListFormat(noOfItems: Option[Int]): String = {
    val example = noOfItems match
      case Some(n) => (1 to n).map(n => s"<item$n>").mkString("\n")
      case None    => "<item1>\n<item2>\n<item3>\n..."
    end example

    s"""The response must be a list of items, one on each line.
       |
       |Example:
       |$example
       |
       |I want nothing else in the response except these items.
       |Also, I do not want any prefix numbers or other characters.""".stripMargin
  }

}
