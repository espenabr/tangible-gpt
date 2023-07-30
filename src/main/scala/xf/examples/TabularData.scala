package xf.examples

import scala.util.Try
import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import xf.examples.Common.{clientResource, createConversationClient, extractKey}
import xf.model.Table.Cell.{BooleanCell, SingleChoiceCell, TextCell}
import xf.model.Table.{Cell, Column, Row}
import xf.model.Table

object TabularData extends IOApp {

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val interactions = createConversationClient(client, extractKey(args))
      for {
        response  <- interactions.chatExpectingTable(description, tableColumns)
        characters = response.value.get.rows.map(toCharacter)
        _         <- Console[IO].println(characters.mkString("\n"))
      } yield ExitCode.Success
    }

  val description = "Characters from the Donald Duck & co. universe."

  val tableColumns: List[Column] = List(
    Column.TextColumn("Name"),
    Column.SingleChoiceColumn("Animal", List("Duck", "Goose", "Mouse", "Dog", "Cow")),
    Column.TextColumn("Occupation"),
    Column.TextColumn("Personality"),
    Column.BooleanColumn("Is old")
  )

  enum Animal:
    case Duck, Goose, Mouse, Dog, Cow

  case class Character(
      name: String,
      animal: Animal,
      occupation: String,
      personality: String,
      isChild: Boolean
  )

  private def toCharacter(row: Row): Option[Character] = {
    val columns = row.columns

    for {
      name        <- stringFromCell(columns.head)
      animalStr   <- stringFromCell(columns(1))
      animal      <- Try(Animal.valueOf(animalStr)).toOption
      occupation  <- stringFromCell(columns(2))
      personality <- stringFromCell(columns(3))
      isOld       <- booleanFromCell(columns(4))
    } yield Character(name, animal, occupation, personality, isOld)
  }

  private def stringFromCell(c: Cell): Option[String] = c match
    case TextCell(s, _)         => Some(s)
    case SingleChoiceCell(c, _) => Some(c)
    case _                      => None

  private def booleanFromCell(c: Cell) = c match
    case BooleanCell(b, _) => Some(b)
    case _                 => None

}
