package xf.interactionhandlers

import xf.model.{InteractionHandler, Table}
import xf.model.Table.Column
import xf.parsers.Parsers
import xf.renderers.Renderers
import xf.renderers.Renderers.{describeColumn, renderTable}

object ExpandTable {

  case class AddColumn(
      table: Table,
      addColumn: Column,
      columnDescription: String
  )

  case class AddRow(
      table: Table,
      rowDescription: String
  )

  val addColumnToTableHandler = new InteractionHandler[AddColumn, Table](
    "Expand this table with another column",
    e => s"""${Renderers.renderTable(e.table)}
            |
            |Expand it with another column: ${describeColumn(e.addColumn)}
            |${e.columnDescription}
            |""".stripMargin,
    e => s"""The response new table should have this format
            |${ResponseAsTable.specifyTableFormat(e.table.columns :+ e.addColumn)}""".stripMargin,
    (e, s) => Parsers.parseTable(e.table.columns :+ e.addColumn)(s)
  )

  val addRowToTableHandler = new InteractionHandler[AddRow, Table](
    "Expand this table with another row",
    e => s"""${renderTable(e.table)}
            |
            |Expand it with another row: ${e.rowDescription}
            |""".stripMargin,
    e => s"""The response new table should have this format
            |${ResponseAsTable.specifyTableFormat(e.table.columns)}""".stripMargin,
    (e, s) => Parsers.parseTable(e.table.columns)(s)
  )

}
