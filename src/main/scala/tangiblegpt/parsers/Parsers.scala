package tangiblegpt.parsers

import cats.implicits.*
import io.circe.Decoder
import io.circe.parser.decode
import tangiblegpt.model.Table
import tangiblegpt.model.Table.{Column, Row}
import tangiblegpt.model.Table.Column.{BooleanColumn, NumberColumn, SingleChoiceColumn, TextColumn}
import tangiblegpt.model.Table.Cell.{BooleanCell, NumberCell, SingleChoiceCell, TextCell}

object Parsers:

  /* General-purpose response parsing */

  def parseTable(columns: List[Column])(s: String): Option[Table] =
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

      Option.when(cells.forall(_.isDefined))(Row(cells.flatMap(_.toList)))
    }

    Option.when(rows.forall(_.isDefined))(Table(columns, rows.flatMap(_.toList)))

  private def parseBoolean(s: String): Option[Boolean] = s.toLowerCase match
    case "false" | "no" | "n" | "0" => Some(false)
    case "true" | "yes" | "y" | "1" => Some(true)
    case _                          => None

  private def parseSingleChoice(s: String, options: List[String]): Option[String] = options.find(_ === s)

  def parseJsonObject[T: Decoder](s: String): Option[T] =
    decode[T](s).toOption

  def parseJsonArray[T: Decoder](s: String): Option[List[T]] =
    decode[List[T]](s).toOption