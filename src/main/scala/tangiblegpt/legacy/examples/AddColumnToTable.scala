package tangiblegpt.legacy.examples

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}
import tangiblegpt.legacy.interactionhandlers.ExpandTable.{AddColumn, AddRow, addColumnToTableHandler, addRowToTableHandler}
import tangiblegpt.legacy.interactionhandlers.ResponseAsTable.{TabularDataRequest, tableHandler}
import tangiblegpt.model.Table
import tangiblegpt.model.Table.Cell.{SingleChoiceCell, TextCell}
import tangiblegpt.model.Table.Column.{NumberColumn, SingleChoiceColumn, TextColumn}
import tangiblegpt.model.Table.{Column, Row}

object AddColumnToTable extends IOApp:

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val ic = createTangibleClient(client, extractKey())
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
