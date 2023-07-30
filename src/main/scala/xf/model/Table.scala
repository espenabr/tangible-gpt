package xf.model

import Table.{Row, Column}

case class Table(
    columns: List[Column],
    rows: List[Row]
)
object Table {

  enum Column:
    case TextColumn(name: String)
    case BooleanColumn(name: String)
    case NumberColumn(name: String)
    case SingleChoiceColumn(name: String, options: List[String])

  enum Cell:
    case TextCell(value: String, column: Column)
    case BooleanCell(value: Boolean, column: Column)
    case NumberCell(value: Double, column: Column)
    case SingleChoiceCell(value: String, column: Column)

  case class Row(columns: List[Cell])

}
