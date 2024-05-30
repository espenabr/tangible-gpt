package tangiblegpt.examples

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}
import tangiblegpt.model.Table
import tangiblegpt.model.Table.Cell.{SingleChoiceCell, TextCell}
import tangiblegpt.model.Table.Column.{NumberColumn, SingleChoiceColumn, TextColumn}
import tangiblegpt.model.Table.{Column, Row}

object AddColumnToTable extends IOApp:

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val tc = createTangibleClient(client, extractKey())
      for
        addedColumn <- tc.expectTableWithAddedColumn(
                         Column.NumberColumn("Average weight"),
                         "Average weight (kg)",
                         exampleTable
                       )
        _           <-
          addedColumn.map { r => Console[IO].println(r.rawMessage) }.getOrElse { IO.raiseError(new RuntimeException()) }
        addedRow    <- tc.expectTableWithAddedRow(addedColumn.toOption.get.value, "Sea Nettle")
        _           <- Console[IO].println("Added Sea Nettle row:\n" + addedRow.toOption.get.rawMessage)
      yield ExitCode.Success
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
