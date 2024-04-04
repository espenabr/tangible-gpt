package xf.model

import Table.{Column, Row}

case class Table(
    columns: List[Column],
    rows: List[Row]
)
object Table:

  enum Column(val name: String):
    case TextColumn(override val name: String)                                extends Column(name)
    case BooleanColumn(override val name: String)                             extends Column(name)
    case NumberColumn(override val name: String)                              extends Column(name)
    case SingleChoiceColumn(override val name: String, options: List[String]) extends Column(name)

  enum Cell:
    case TextCell(value: String, column: Column)
    case BooleanCell(value: Boolean, column: Column)
    case NumberCell(value: Double, column: Column)
    case SingleChoiceCell(value: String, column: Column)

  case class Row(columns: List[Cell])
