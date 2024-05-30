package tangiblegpt.examples

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}
import tangiblegpt.model.Table
import tangiblegpt.model.Table.Cell.{BooleanCell, SingleChoiceCell, TextCell}
import tangiblegpt.model.Table.{Cell, Column, Row}

import scala.util.Try

object TabularData extends IOApp:

  /*
   * Ask for data in a tabular format with specific data types per column
   * Also, example of how the Table type can be mapped to case classes
   */
  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val tc = createTangibleClient(client, extractKey())
      for
        response  <- tc.expectTable("10 characters from the Donald Duck & co. universe.", tableColumns)
        characters = response.toOption.map(_.value).map(_.rows.map(toCharacter)).getOrElse(List.empty)
        _         <- Console[IO].println(characters.mkString("\n"))
      yield ExitCode.Success
    }

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

  private def toCharacter(row: Row): Option[Character] =
    val columns = row.columns
    for
      name        <- stringFromCell(columns.head)
      animalStr   <- stringFromCell(columns(1))
      animal      <- Try(Animal.valueOf(animalStr)).toOption
      occupation  <- stringFromCell(columns(2))
      personality <- stringFromCell(columns(3))
      isOld       <- booleanFromCell(columns(4))
    yield Character(name, animal, occupation, personality, isOld)

  private def stringFromCell(c: Cell): Option[String] = c match
    case TextCell(s, _)         => Some(s)
    case SingleChoiceCell(c, _) => Some(c)
    case _                      => None

  private def booleanFromCell(c: Cell) = c match
    case BooleanCell(b, _) => Some(b)
    case _                 => None
