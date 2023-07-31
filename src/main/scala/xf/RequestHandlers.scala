package xf

import xf.model.RequestHandler
import xf.model.Table
import xf.model.Table.{Cell, Row}
import xf.model.Table.Cell.{BooleanCell, NumberCell, SingleChoiceCell, TextCell}

object RequestHandlers {

  val listRequestHandler = new RequestHandler[List[String]](l => s"""
         |${l.mkString("\n")}
         |""".stripMargin)

  val tableRequestHandler = new RequestHandler[Table](table => s"""
         |csv format (semicolon separated) with columns: ${table.columns.map(_.name).mkString(";")}:
         |${table.rows.map(serializedRow).mkString("\n")}
         |
         |""".stripMargin)

  private def serializedRow(row: Row): String =
    row.columns.map(serializeCell).mkString(";")

  private def serializeCell(cell: Cell): String =
    cell match {
      case TextCell(v, _)         => v
      case NumberCell(n, _)       => n.toString
      case BooleanCell(b, _)      => b.toString
      case SingleChoiceCell(c, _) => c
    }

}