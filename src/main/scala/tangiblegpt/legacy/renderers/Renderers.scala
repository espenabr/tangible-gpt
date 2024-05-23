package tangiblegpt.legacy.renderers

import tangiblegpt.model.Table
import tangiblegpt.model.Table.{Cell, Column, Row}
import tangiblegpt.model.Table.Cell.{BooleanCell, NumberCell, SingleChoiceCell, TextCell}
import tangiblegpt.model.Table.Column.{BooleanColumn, NumberColumn, SingleChoiceColumn, TextColumn}

object Renderers:

  /* General-purpose request rendering */

  def renderTable(table: Table) =
    s"""csv format (semicolon separated) with columns: ${table.columns.map(_.name).mkString(";")}:
       |${table.rows.map(serializedRow).mkString("\n")}
       |""".stripMargin

  private def serializedRow(row: Row): String =
    row.columns.map(serializeCell).mkString(";")

  private def serializeCell(cell: Cell): String =
    cell match {
      case TextCell(v, _)         => v
      case NumberCell(n, _)       => n.toString
      case BooleanCell(b, _)      => b.toString
      case SingleChoiceCell(c, _) => c
    }

  def describeColumn(c: Column) = c match {
    case BooleanColumn(name)               => s"$name: Boolean (true or false)"
    case NumberColumn(name)                => s"$name: Number (any number including decimal)"
    case TextColumn(name)                  => s"$name: String"
    case SingleChoiceColumn(name, options) => s"$name: One of the following values: ${options.mkString(", ")}"
  }
