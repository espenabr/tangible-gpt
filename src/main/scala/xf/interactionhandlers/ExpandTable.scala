package xf.interactionhandlers

import xf.model.{InteractionHandler, Table}
import xf.model.Table.Column
import xf.parsers.Parsers
import xf.renderers.Renderers
import xf.renderers.Renderers.{describeColumn, renderTable}

object ExpandTable:

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
    e => s"""${Renderers.renderTable(e.table)}
            |
            |Expand it with another column: ${describeColumn(e.addColumn)}
            |${e.columnDescription}
            |""".stripMargin,
    e => s"""The response new table should have this format
            |${ResponseAsTable.specifyTableFormat(e.table.columns :+ e.addColumn)}""".stripMargin,
    (e, s) => Parsers.parseTable(e.table.columns :+ e.addColumn)(s),
    objective = Some("Expand this table with another column")
  )

  val addRowToTableHandler = new InteractionHandler[AddRow, Table](
    e => s"""${renderTable(e.table)}
            |
            |Expand it with another row: ${e.rowDescription}
            |""".stripMargin,
    e => s"""The response new table should have this format
            |${ResponseAsTable.specifyTableFormat(e.table.columns)}""".stripMargin,
    (e, s) => Parsers.parseTable(e.table.columns)(s),
    objective = Some("Expand this table with another row"),

  )
