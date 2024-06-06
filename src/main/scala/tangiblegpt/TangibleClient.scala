package tangiblegpt

import tangiblegpt.gpt.GptApiClient
import cats.effect.Concurrent
import cats.implicits.*
import io.circe
import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Decoder, Encoder}
import io.circe.parser.decode
import io.circe.syntax.*
import tangiblegpt.gpt.GptApiClient.Common.{Message, Role}
import tangiblegpt.gpt.GptApiClient.Common.Message.{ContentMessage, ResultFromToolMessage}
import tangiblegpt.gpt.GptApiClient.Common.Role.User
import tangiblegpt.gpt.GptApiClient.Request.Property.{EnumProperty, IntegerProperty, StringProperty}
import tangiblegpt.gpt.GptApiClient.Request.{Parameters, Property, RequestFunction, Tool}
import tangiblegpt.gpt.GptApiClient.Response.CompletionResponse
import tangiblegpt.model.FailedInteraction.ParseError
import tangiblegpt.model.Param.{EnumParam, IntegerParam, StringParam}
import tangiblegpt.model.ReasoningStrategy.{Simple, SuggestMultipleAndPickOne, ThinkStepByStep}
import tangiblegpt.model.{
  AnswerToQuestionFromGpt,
  FailedInteraction,
  FunctionCall,
  ItemGroup,
  QuestionFromGpt,
  QuestionType,
  ReasoningStrategy,
  Table,
  TangibleEitherResponse,
  TangibleOptionResponse,
  TangibleResponse
}
import tangiblegpt.gpt.GptApiClient.Response.Choice.{StopChoice, ToolCallsChoice}
import tangiblegpt.model.Table.{Cell, Column, Row}
import tangiblegpt.model.QuestionType.*
import tangiblegpt.model.QuestionFromGpt.*
import tangiblegpt.model.AnswerToQuestionFromGpt.*

import scala.util.Try

class TangibleClient[F[_]: Concurrent](gptApiClient: GptApiClient[F]):

  def expectJson[R](
      prompt: String,
      example: R,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  )(implicit
      decoder: Decoder[R],
      encoder: Encoder[R]
  ): F[Either[FailedInteraction, TangibleResponse[R]]] =
    def encodeToJson(value: R): String = value.asJson.noSpaces

    val responseFormatDescription =
      s"""The response must be in valid JSON and only JSON, nothing else
         |
         |Example:
         |${encodeToJson(example)}""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      decode[R](r.value)
        .map { decoded => TangibleResponse[R](decoded, r.value, r.history) }
        .leftMap(_ => ParseError(r.value, r.history))
    }

  def expectJsonOption[R](
      prompt: String,
      example: R,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  )(implicit
      decoder: Decoder[R],
      encoder: Encoder[R]
  ): F[Either[FailedInteraction, TangibleOptionResponse[R]]] =
    def encodeToJson(value: R): String = value.asJson.noSpaces
    val responseFormatDescription      =
      s"""If you don't, know the answer, simply say "I don't know". Otherwise, the response must be in valid JSON and
         |only JSON, nothing else.
         |
         |Example:
         |${encodeToJson(example)}""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      if iDontKnow(r) then Right(TangibleOptionResponse[R](None, r.value, r.history))
      else
        decode[R](r.value)
          .map { decoded => TangibleOptionResponse[R](Some(decoded), r.value, r.history) }
          .leftMap(_ => ParseError(r.value, r.history))
    }

  private def iDontKnow(response: TangibleResponse[String]): Boolean =
    response.value.toLowerCase.trim.contains("i don't know")

  def expectJsonEither[L, R](
      prompt: String,
      leftExample: L,
      rightExample: R,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  )(implicit
      leftDecoder: Decoder[L],
      leftEncoder: Encoder[L],
      rightDecoder: Decoder[R],
      rightEncoder: Encoder[R]
  ): F[Either[FailedInteraction, TangibleEitherResponse[L, R]]] =
    def encodeLeftToJson(value: L): String  = value.asJson.noSpaces
    def encodeRightToJson(value: R): String = value.asJson.noSpaces

    val responseFormatDescription =
      s"""The response must be valid JSON and JSON only.
         |Depending on the answer, it may either have this format (example): ${encodeLeftToJson(leftExample)}
         |Or this format (example): ${encodeRightToJson(rightExample)}""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      val decodedLeft: Either[circe.Error, L]  = decode[L](r.value)
      val decodedRight: Either[circe.Error, R] = decode[R](r.value)
      def toResponse(value: Either[L, R])      = TangibleEitherResponse(value, r.value, r.history)

      (decodedLeft.toOption.map(l => toResponse(Left(l))) orElse decodedRight.toOption.map(r => toResponse(Right(r))))
        .toRight(ParseError(r.value, r.history))
    }

  def expectPlainText(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[TangibleResponse[String]] =
    interact(
      initialPrompt(reasoningStrategy, prompt, None),
      history,
      functionCalls,
      reasoningStrategy,
      None,
      withFollowupQuestions
    ).map { r => TangibleResponse[String](r.value, r.value, r.history) }

  def expectPlainTextOption(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[Either[FailedInteraction, TangibleOptionResponse[String]]] =
    val responseFormatDescription =
      s"""If you don't know the answer, simply say "I don't know"""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      val value = if iDontKnow(r) then None else Some(r.value)
      Right(TangibleOptionResponse[String](value, r.value, r.history))
    }

  def expectBoolean(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[Either[FailedInteraction, TangibleResponse[Boolean]]] =
    val responseFormatDescription =
      """I only want a yes or no answer, nothing else. Reply with either "yes" or "no""""

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      parseBoolean(r.value) match
        case Some(b) => Right(TangibleResponse[Boolean](b, r.value, r.history))
        case None    => Left(ParseError(r.value, r.history))
    }

  def expectBooleanOption(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[Either[FailedInteraction, TangibleOptionResponse[Boolean]]] =
    val responseFormatDescription =
      s"""If you don't know the answer, simply reply with "I don't know", nothing else.
         |I only want a yes or no answer, nothing else. Reply with either "yes" or "no"""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      def toResponse(value: Option[Boolean]) =
        TangibleOptionResponse[Boolean](value, r.value, r.history)

      if iDontKnow(r) then Right(toResponse(None))
      else
        parseBoolean(r.value) match
          case Some(b) => Right(toResponse(Some(b)))
          case None    => Left(ParseError(r.value, r.history))
    }

  def expectDouble(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[Either[FailedInteraction, TangibleResponse[Double]]] =
    val responseFormatDescription = "I only want a number (all digits) as an answer, nothing else."

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      Try { r.value.toDouble }.toOption
        .map { d => Right(TangibleResponse(d, r.value, r.history)) }
        .getOrElse(Left(ParseError(r.value, r.history)))
    }

  def expectDoubleOption(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[Either[FailedInteraction, TangibleOptionResponse[Double]]] =
    val responseFormatDescription =
      s"""If you don't know the answer, simply reply with "I don't know", nothing else.
         |I only want a number as an answer, nothing else.""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      val content                           = r.value
      def toResponse(value: Option[Double]) = TangibleOptionResponse(value, content, r.history)

      if iDontKnow(r) then Right(toResponse(None))
      else
        Try { content.toDouble }.toOption
          .map { d => Right(TangibleOptionResponse(Some(d), content, r.history)) }
          .getOrElse(Left(ParseError(content, r.history)))
    }

  def expectEnumCase[T](
      prompt: String,
      options: List[T],
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[Either[FailedInteraction, TangibleResponse[T]]] =
    val responseFormatDescription =
      s"""I want you to respond with one of the following values, nothing else:
         |${options.map(_.toString).mkString(", ")}
         |""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      val content              = r.value
      def toResponse(value: T) = TangibleResponse(value, content, r.history)

      options
        .find(_.toString.toLowerCase() === content.trim.toLowerCase())
        .map(e => Right(toResponse(e)))
        .getOrElse(Left(ParseError(content, history)))
    }

  def expectEnumCaseOption[T](
      prompt: String,
      options: List[T],
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[Either[FailedInteraction, TangibleOptionResponse[T]]] =
    val responseFormatDescription =
      s"""If you don't know, simply reply "I don't know", nothing else.
         |Otherwise, I want you to respond with one of the following values, nothing else:
         |${options.map(_.toString).mkString(", ")}
         |""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      val content = r.value

      def toResponse(value: Option[T]) = TangibleOptionResponse(value, content, r.history)

      if iDontKnow(r) then Right(toResponse(None))
      else
        options
          .find(_.toString.toLowerCase() === content.trim.toLowerCase())
          .map(e => Right(toResponse(Some(e))))
          .getOrElse(Left(ParseError(content, history)))
    }

  def expectEnumCases[T](
      prompt: String,
      options: List[T],
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[Either[FailedInteraction, TangibleResponse[Set[T]]]] =
    val responseFormatDescription =
      s"""Given the following options:
         |${options.map(_.toString).mkString(", ")}
         |I want you to respond with those that apply. If none of them apply, just say "None".
         |I want a list of options on a single line, separated by comma, and nothing else in the response.""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      val content = r.value

      def toResponse(value: Set[T]) = TangibleResponse(value, content, r.history)
      val splitted: Set[String]     = content.split(",").toList.map(_.toLowerCase.strip).toSet
      val allSelectionsValid        = splitted.forall { o => options.exists(_.toString === o) }
      val result                    = options.filter { o => splitted.exists(_ === o.toString.toLowerCase) }.toSet

      if allSelectionsValid then Right(toResponse(result))
      else Left(FailedInteraction.ParseError(content, history))
    }

  def expectGroups(
      items: List[String],
      groupNames: Option[Set[String]],
      groupingCriteria: Option[String],
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[Either[FailedInteraction, TangibleResponse[List[ItemGroup]]]] =
    val prompt = groupNames match
      case Some(gn) =>
        s"""I want you to put some items into different groups.
           |
           |These groups are: ${gn.mkString(", ")}
           |${groupingCriteria.map(gc => s"Items should be grouped by the following criteria: $gc")}
           |
           |Here are the items that should be distributed in the right groups:
           |
           |${items.mkString("\n")}""".stripMargin
      case None     =>
        s"""I want you to put some items into different groups.
           |Make up some sensible names for these groups.
           |${groupingCriteria.map(gc => s"Items should be grouped by the following criteria: $gc")}
           |
           |Here are the items that should be distributed in the right groups:
           |
           |${items.mkString("\n")}""".stripMargin

    val example: List[ItemGroup] =
      List(
        ItemGroup("group1", List("item1", "item2")),
        ItemGroup("group2", List("item3"))
      )

    given Codec[ItemGroup]        = deriveCodec
    val responseFormatDescription =
      s"""The response must be in valid JSON and only JSON, nothing else
         |
         |Example:
         |${example.asJson}""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      decode[List[ItemGroup]](r.value)
        .map { decoded => TangibleResponse[List[ItemGroup]](decoded, r.value, r.history) }
        .leftMap(_ => ParseError(r.value, r.history))
    }

  def expectItems(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[Either[FailedInteraction, TangibleResponse[List[String]]]] =
    val responseFormatDescription = "I only want a list of items. Each item on its own line. Nothing else."

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      val items = r.value.split("\n").toList.map(_.strip()).filter(_.nonEmpty)
      if items.nonEmpty then Right(TangibleResponse[List[String]](items, r.value, r.history))
      else Left(ParseError(r.value, r.history))
    }

  def expectSorted(
      items: List[String],
      sortingCriteria: Option[String],
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[Either[FailedInteraction, TangibleResponse[List[String]]]] =
    val prompt = sortingCriteria match
      case Some(sc) =>
        s"""I want you to sort a list of items based on the following criteria: $sc
           |
           |Here are the items to be sorted:
           |${items.mkString("\n")}""".stripMargin
      case None     =>
        s"""I want you to sort the following items in the most obvious way:
           |${items.mkString("\n")}""".stripMargin

    val responseFormatDescription =
      s"""The response must be a sorted JSON array of strings (items), nothing else""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      decode[List[String]](r.value)
        .map { decoded => TangibleResponse[List[String]](decoded, r.value, r.history) }
        .leftMap(_ => ParseError(r.value, r.history))
    }

  def expectFiltered(
      items: List[String],
      predicate: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[Either[FailedInteraction, TangibleResponse[List[String]]]] =
    val prompt =
      s"""I have a list of items that I need to filter.
         |Only include the items that adhere to the following predicate: $predicate
         |
         |The items are:
         |${items.mkString("\n")}""".stripMargin

    val responseFormatDescription =
      s"""The response must be a filtered JSON array of strings (items), nothing else""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      decode[List[String]](r.value)
        .map { decoded => TangibleResponse[List[String]](decoded, r.value, r.history) }
        .leftMap(_ => ParseError(r.value, r.history))
    }

  def expectTable(
      prompt: String,
      columns: List[Column],
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[Either[FailedInteraction, TangibleResponse[Table]]] =
    val responseFormatDescription =
      s"""The response must be CSV format (semicolon separated) with columns: ${columns.map(_.name).mkString(";")}
         |No header row, just data
         |
         |Columns:
         |${columns.map(describeColumn).mkString("\n")}""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      parseTable(columns)(r.value)
        .map { parsed => Right(TangibleResponse(parsed, r.rawMessage, r.history)) }
        .getOrElse { Left(ParseError(r.rawMessage, r.history)) }
    }

  def expectTableWithAddedColumn(
      columnToAdd: Column,
      intention: String,
      table: Table,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[Either[FailedInteraction, TangibleResponse[Table]]] =
    val prompt                    =
      s"""${renderTable(table)}
         |
         |Expand this table with another column:
         |${describeColumn(columnToAdd)}
         |
         |$intention""".stripMargin
    val resultColumns             = table.columns :+ columnToAdd
    val responseFormatDescription =
      s"""The response must be CSV format (semicolon separated) with columns: ${resultColumns.map(_.name).mkString(";")}
         |No header row, just data
         |
         |Columns:
         |${table.columns.map(describeColumn).mkString("\n")}""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      parseTable(table.columns :+ columnToAdd)(r.value)
        .map { parsed => Right(TangibleResponse(parsed, r.rawMessage, r.history)) }
        .getOrElse {
          Left(ParseError(r.rawMessage, r.history))
        }
    }

  def expectTableWithAddedRow(
      table: Table,
      rowDescription: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple,
      withFollowupQuestions: Option[FollowupQuestions] = None
  ): F[Either[FailedInteraction, TangibleResponse[Table]]] =
    val prompt =
      s"""${renderTable(table)}
         |
         |Expand this table with another row:
         |$rowDescription""".stripMargin

    val responseFormatDescription =
      s"""The response must be CSV format (semicolon separated) with columns: ${table.columns.map(_.name).mkString(";")}
         |No header row, just data
         |
         |Columns:
         |${table.columns.map(describeColumn).mkString("\n")}""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription),
      withFollowupQuestions
    ).map { r =>
      parseTable(table.columns)(r.value)
        .map { parsed => Right(TangibleResponse(parsed, r.rawMessage, r.history)) }
        .getOrElse {
          Left(ParseError(r.rawMessage, r.history))
        }
    }

  private def plainTextChat(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[TangibleResponse[String]] =
    val message  = userContentMessage(prompt)
    val messages = history :+ message

    gptApiClient.chatCompletions(messages).map { response =>
      val reply = response.choices.last.message
      TangibleResponse[String](
        reply.content,
        reply.content,
        history :+ message :+ reply
      )
    }

  private def parseBoolean(s: String): Option[Boolean] = s.toLowerCase match
    case "false" | "no" | "n" | "0" => Some(false)
    case "true" | "yes" | "y" | "1" => Some(true)
    case _                          => None

  private def parseSingleChoice(s: String, options: List[String]): Option[String] = options.find(_ === s)

  private def renderTable(table: Table) =
    s"""csv format (semicolon separated) with columns: ${table.columns.map(_.name).mkString(";")}:
       |${table.rows.map(serializedRow).mkString("\n")}
       |""".stripMargin

  private def serializedRow(row: Row): String =
    row.columns.map(serializeCell).mkString(";")

  private def serializeCell(cell: Cell): String =
    cell match {
      case Cell.TextCell(v, _)         => v
      case Cell.NumberCell(n, _)       => n.toString
      case Cell.BooleanCell(b, _)      => b.toString
      case Cell.SingleChoiceCell(c, _) => c
    }

  private def parseTable(columns: List[Column])(s: String): Option[Table] =
    val lines = s.split("\n").toList.filter(_.contains(";"))
    val rows  = lines.map { line =>
      val parts = line.split(";").toList.map(_.stripPrefix("\"").stripSuffix("\""))
      val cells = columns.indices.zip(columns).toList.map { case (index, column) =>
        val part: String = parts(index)
        column match
          case Column.BooleanColumn(_)               => parseBoolean(part).map(p => Cell.BooleanCell(p, column))
          case Column.NumberColumn(_)                => part.toDoubleOption.map(n => Cell.NumberCell(n, column))
          case Column.TextColumn(_)                  => Some(Cell.TextCell(part, column))
          case Column.SingleChoiceColumn(_, options) =>
            parseSingleChoice(part, options).map(s => Cell.SingleChoiceCell(s, column))
      }
      Option.when(cells.forall(_.isDefined))(Row(cells.flatMap(_.toList)))
    }
    Option.when(rows.forall(_.isDefined))(Table(columns, rows.flatMap(_.toList)))

  private def describeColumn(c: Column) = c match
    case Column.BooleanColumn(name)               => s"$name: Boolean (true or false)"
    case Column.NumberColumn(name)                => s"$name: Number (any number including decimal)"
    case Column.TextColumn(name)                  => s"$name: String"
    case Column.SingleChoiceColumn(name, options) => s"$name: One of the following values: ${options.mkString(", ")}"

  case class FollowupQuestions(
      questionType: QuestionType,
      noOfQuestions: Option[Int],
      collectAnswers: List[QuestionFromGpt] => F[List[AnswerToQuestionFromGpt]]
  )

  private def interact(
      prompt: String,
      history: List[Message],
      functionCalls: List[FunctionCall[F]],
      reasoningStrategy: ReasoningStrategy,
      responseFormatDescription: Option[String],
      withFollowupQuestions: Option[FollowupQuestions]
  ): F[TangibleResponse[String]] =
    withFollowupQuestions match
      case Some(fq) =>
        val followupQuestionsPrompt =
          s"""$prompt
             |
             |${toFollowupQuestionsPrompt(fq.questionType, fq.noOfQuestions)}""".stripMargin

        def answersPrompt(answers: List[AnswerToQuestionFromGpt]) =
          s"""${renderAnswers(answers)}
             |
             |$prompt""".stripMargin

        for
          questionsResponse <- plainTextChat(followupQuestionsPrompt, history = history)
          parsedQuestions    = parseQuestions(fq.questionType, questionsResponse.value).getOrElse(List.empty)
          answers           <- fq.collectAnswers(parsedQuestions)
          response          <- withPossibleFunctionCalls(
                                 answersPrompt(answers),
                                 history,
                                 functionCalls
                               )
          finalResponse     <- afterPossibleReasoning(response, reasoningStrategy, responseFormatDescription)
        yield finalResponse
      case None     =>
        for
          response      <- withPossibleFunctionCalls(prompt, history, functionCalls)
          finalResponse <- afterPossibleReasoning(response, reasoningStrategy, responseFormatDescription)
        yield finalResponse

  private def withPossibleFunctionCalls(
      prompt: String,
      history: List[Message],
      functionCalls: List[FunctionCall[F]]
  ): F[TangibleResponse[String]] =
    if functionCalls.isEmpty then plainTextChat(prompt, history = history)
    else chatWithFunctionCalls(prompt, functionCalls, history)

  private def chatWithFunctionCalls(
      prompt: String,
      functionCalls: List[FunctionCall[F]],
      history: List[Message] = List.empty
  ): F[TangibleResponse[String]] =
    val message  = userContentMessage(prompt)
    val messages = history :+ message
    val tools    = functionCallTools(functionCalls)
    for
      initialResponse <- gptApiClient.chatCompletions(messages, Some(tools))
      response        <-
        callFunctionIfApplicable(
          initialResponse,
          functionCalls,
          message +: initialResponse.choices.map(_.message)
        )
    yield
      val reply: Message         = response.choices.last.message
      val history: List[Message] = message +: response.choices.map(_.message)
      TangibleResponse[String](reply.content, reply.content, history)

  private def callFunctionIfApplicable(
      initialResponse: CompletionResponse,
      functionCalls: List[FunctionCall[F]],
      history: List[Message]
  ): F[CompletionResponse] =
    initialResponse.choices.last match
      case cm: StopChoice                  => initialResponse.pure[F]
      case ToolCallsChoice(index, message) =>
        val returnMessages: F[List[Message]] =
          (for
            toolCall     <- message.toolCalls
            functionCall <- functionCalls.filter(_.name === toolCall.function.name)
          yield
            val result: F[String] = functionCall.function(toolCall.function.arguments)
            result.map(r => ResultFromToolMessage(Role.Tool, toolCall.function.name, r, toolCall.id))
          ).sequence

        returnMessages.flatMap(rm =>
          gptApiClient.chatCompletions(history ++ rm, Some(functionCallTools(functionCalls)))
        )

  private def afterPossibleReasoning(
      response: TangibleResponse[String],
      reasoningStrategy: ReasoningStrategy,
      responseFormatDescription: Option[String]
  ): F[TangibleResponse[String]] =
    reasoningStrategy match
      case Simple                    => response.pure[F]
      case ThinkStepByStep           =>
        expectPlainText(
          responseFormatDescription
            .map(rfd => s"""Give me an answer.
                             |
                             |$rfd""".stripMargin)
            .getOrElse("Give me an answer."),
          response.history
        )
      case SuggestMultipleAndPickOne =>
        expectPlainText(
          responseFormatDescription
            .map(rfd => s"""Pick the best answer.
                             |
                             |$responseFormatDescription""".stripMargin)
            .getOrElse("Pick the best answer."),
          response.history
        )

  private def initialPrompt(
      reasoningStrategy: ReasoningStrategy,
      prompt: String,
      responseFormatDescription: Option[String]
  ): String =
    reasoningStrategy match
      case Simple                    =>
        responseFormatDescription match
          case Some(formatDescription) =>
            s"""$prompt
               |
               |$formatDescription""".stripMargin
          case None                    => prompt
      case ThinkStepByStep           =>
        s"""$prompt
           |
           |Let's think step by step""".stripMargin
      case SuggestMultipleAndPickOne =>
        s"""$prompt
           |
           |Give me some alternative answers to this that make sense. Enumerate them.""".stripMargin

  private def userContentMessage(s: String) = ContentMessage(User, s)

  private def functionCallTools(functionCalls: List[FunctionCall[F]]) = functionCalls.map(fc =>
    Tool(
      RequestFunction(
        fc.name,
        Some(fc.description),
        Parameters(
          fc.params.map {
            case IntegerParam(name, description)     => (name, IntegerProperty(description))
            case StringParam(name, description)      => (name, StringProperty(description))
            case EnumParam(name, description, _enum) => (name, EnumProperty(description, _enum))
          }.toMap,
          List.empty
        )
      )
    )
  )

  private def parseQuestions(questionType: QuestionType, s: String): Option[List[QuestionFromGpt]] =
    val lines     = s.split("\n")
    val questions = (1 to lines.length).toList.zip(lines).map { case (idx, line) =>
      questionType match {
        case YesNoQuestions             => QuestionFromGpt.BooleanQuestionFromGpt(idx, line)
        case TextQuestions              => QuestionFromGpt.TextQuestionFromGpt(idx, line)
        case SingleChoiceQuestions(_)   =>
          val parts = line.split(";").toList
          QuestionFromGpt.SingleChoiceQuestionFromGpt(idx, parts.head, parts.tail)
        case MultipleChoiceQuestions(_) =>
          val parts = line.split(";").toList
          QuestionFromGpt.MultipleChoiceQuestionFromGpt(idx, parts.head, parts.tail)
      }
    }
    Some(questions) // TODO None if parse error

  private def toFollowupQuestionsPrompt(questionType: QuestionType, noOfQuestions: Option[Int]) =
    val describePossibleAnswers = questionType match {
      case YesNoQuestions                       =>
        s"""All questions should only have "yes" or "no" answers. ${noOfQuestions.map(n => s"$n questions total.")}"""
      case TextQuestions                        => noOfQuestions.map(n => s"$n questions total.").getOrElse("")
      case SingleChoiceQuestions(noOfOptions)   => questionsWithChoicesPrompt(noOfQuestions, noOfOptions)
      case MultipleChoiceQuestions(noOfOptions) => questionsWithChoicesPrompt(noOfQuestions, noOfOptions)
    }

    s"""
       |I want a list of questions, one on each line.
       |No lines should be prefixed with a number or any other character to indicate it's an item in a list, only the item itself and the options.
       |$describePossibleAnswers
       |""".stripMargin

  private def questionsWithChoicesPrompt(noOfQuestions: Option[Int], noOfOptions: Option[Int]) =
    val example = noOfOptions match
      case Some(n) =>
        s"""<question1>;${(1 to n).map(n => s"<option$n>").mkString(";")}
           |<question2>;${(1 to n).map(n => s"<option$n>").mkString(";")}
           |...""".stripMargin
      case None    =>
        s"""<question1>;<option1>;<option2>;<option3>
           |<question2>;<option1>;<option2>;<option3>
           |...""".stripMargin

    s"""The response must be a list of questions, one line each.
       |Each line should start with the question, continuing with a list of possible options as answer, all separated by ; (semicolon).
       |No lines should be prefixed with a number or any other character to indicate it's an item in a list, only the item itself and the options.
       |${noOfOptions
        .map(n => s"Each questions should have $n possible answers each.")
        .getOrElse("Each question should have a number of possible answers each.")}
       |
       |Example:
       |$example
       |
       |I want nothing else in the response except these questions with options.""".stripMargin

  private def renderAnswers(answers: List[AnswerToQuestionFromGpt]) =
    answers.map(describeAnswer).mkString("\n")

  private def describeAnswer(answer: AnswerToQuestionFromGpt) = {
    val (q, a) = answer match
      case AnswerToBooleanQuestionFromGpt(question, answer)               => (question, yesNo(answer))
      case AnswerToTextQuestionFromQpt(question, answer)                  => (question, answer)
      case AnswerToSingleChoiceQuestionFromGpt(question, index)           =>
        (question, question.options(index))
      case AnswerToMultipleChoiceQuestionFromGpt(question, answerIndices) =>
        (question, answerIndices.map(_ + 1).mkString(", "))

    s"""Question: ${q.question}
       |Answer: $a
       |""".stripMargin
  }

  private def yesNo(b: Boolean) = if b then "yes" else "no"
