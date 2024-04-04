package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import xf.examples.Common.{clientResource, createInteractionClient, extractKey}
import xf.interactionhandlers.ExpandTable.{addColumnToTableHandler, addRowToTableHandler, AddColumn, AddRow}
import xf.interactionhandlers.ResponseAsTable.{tableHandler, TabularDataRequest}
import xf.model.Table
import xf.model.Table.Column
import xf.model.Table.Column.{NumberColumn, SingleChoiceColumn, TextColumn}
import xf.model.Table.Row
import xf.model.Table.Cell.{SingleChoiceCell, TextCell}

object AddColumnToTable extends IOApp:

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val ic = createInteractionClient(client, extractKey(args))
      for {
        addedColumn <- ic.chat(
                         AddColumn(
                           exampleTable,
                           NumberColumn("Average weight"),
                           "Average weight (kg)"
                         ),
                         addColumnToTableHandler
                       )
        _           <- Console[IO].println("Added weight column:\n" + addedColumn.rawMessage)
        addedRow    <- ic.chat(
                         AddRow(addedColumn.value.get, "Sea Nettle"),
                         addRowToTableHandler
                       )
        _           <- Console[IO].println("Added Sea Nettle row:\n" + addedRow.rawMessage)
      } yield ExitCode.Success
    }

  val seaAnimalColumn: TextColumn    = TextColumn("Sea animal")
  val typeColumn: SingleChoiceColumn = SingleChoiceColumn("Type", List("Fish", "Shellfish", "Jellyfish"))

  val exampleTable: Table =
    Table(
      List(seaAnimalColumn, typeColumn),
      List(
        Row(List(TextCell("Cod", seaAnimalColumn), SingleChoiceCell(typeColumn.options.head, typeColumn))),
        Row(List(TextCell("Salmon", seaAnimalColumn), SingleChoiceCell(typeColumn.options.head, typeColumn))),
        Row(List(TextCell("Crab", seaAnimalColumn), SingleChoiceCell(typeColumn.options(1), typeColumn))),
        Row(List(TextCell("Lobster", seaAnimalColumn), SingleChoiceCell(typeColumn.options(1), typeColumn)))
      )
    )

  val tableColumns: List[Column] = List(
    Column.TextColumn("Name"),
    Column.SingleChoiceColumn("Animal", List("Duck", "Goose", "Mouse", "Dog", "Cow")),
    Column.TextColumn("Occupation"),
    Column.TextColumn("Personality"),
    Column.BooleanColumn("Is old")
  )
