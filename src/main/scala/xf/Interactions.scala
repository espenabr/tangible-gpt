package xf

import scala.util.Try
import xf.gpt.GptApiClient
import GptApiClient.Model.*
import cats.effect.Concurrent
import cats.implicits.*
import GptApiClient.Model.Role.{Assistant, User}
import xf.Interactions.Model.QuestionFromGpt.{
  BooleanQuestionFromGpt,
  MultipleChoiceQuestionFromGpt,
  NumberQuestionFromGpt,
  SingleChoiceQuestionFromGpt,
  TextQuestionFromGpt
}
import xf.Interactions.Model.ExpectedQuestion.{
  ExpectedBooleanQuestion,
  ExpectedMultipleChoiceQuestion,
  ExpectedNumberQuestion,
  ExpectedSingleChoiceQuestion,
  ExpectedTextQuestion
}
import xf.Interactions.Model.{
  AnswerToQuestionFromGpt,
  Cell,
  ChatResponse,
  Column,
  Data,
  ExpectedFormat,
  ExpectedQuestion,
  MessageExchange,
  QuestionFromGpt,
  QuestionsFromGptResponse,
  Row,
  SimpleChatResponse,
  Table
}
import xf.Interactions.Model.Cell.{BooleanCell, NumberCell, SingleChoiceCell, TextCell}
import xf.Interactions.Model.Data.{BooleanData, ListData, NumberData, ParseError, TableData, TextData}
import xf.Interactions.Model.ExpectedFormat.{BooleanFormat, ListFormat, NumberFormat, TableFormat, TextFormat}
import xf.Interactions.Model.AnswerToQuestionFromGpt.{
  AnswerToBooleanQuestionFromGpt,
  AnswerToMultipleChoiceQuestionFromGpt,
  AnswerToNumberQuestionFromGpt,
  AnswerToSingleChoiceQuestionFromGpt,
  AnswerToTextQuestionFromQpt
}
import xf.Interactions.Model.Column.{BooleanColumn, NumberColumn, SingleChoiceColumn, TextColumn}

class Interactions[F[_]: Concurrent](gptApiClient: GptApiClient[F]) {

  def simpleChat(message: String, history: List[MessageExchange] = List.empty): F[SimpleChatResponse] = {
    val messages = appendToHistory(history, message)
    gptApiClient.chatCompletions(messages).map { response =>
      val reply = latestMessage(response)
      SimpleChatResponse(reply, history :+ MessageExchange(message, reply))
    }
  }

  def chat(
      message: String,
      format: ExpectedFormat = TextFormat,
      history: List[MessageExchange] = List.empty
  ): F[ChatResponse] = {
    val formatDescription = format match
      case TextFormat           => ""
      case ListFormat(n)        => Prompting.specifyListFormat(n)
      case BooleanFormat        => Prompting.specifyBooleanFormat
      case NumberFormat         => Prompting.specifyNumberFormat
      case TableFormat(columns) => Prompting.specifyTableFormat(columns)
    end formatDescription

    val prompt =
      s"""$message
         |
         |$formatDescription""".stripMargin

    simpleChat(prompt, history).map { response =>
      val reply  = response.message
      val parsed = parseMessage(reply, format)
      ChatResponse(parsed, reply, history :+ MessageExchange(prompt, reply))
    }
  }

  def requestQuestionsFromGpt(
      message: String,
      questionType: ExpectedQuestion,
      noOfQuestions: Option[Int] = None,
      history: List[MessageExchange] = List.empty
  ): F[QuestionsFromGptResponse] = {
    val questionSpecification = questionType match {
      case ExpectedBooleanQuestion           => Prompting.yesNoQuestionsOnly
      case ExpectedTextQuestion              => ""
      case ExpectedNumberQuestion            => Prompting.numericQuestionsOnly
      case ExpectedSingleChoiceQuestion(n)   =>
        s"""Each question (item) should have a number of possible options as an answer, from which only on can be selected.
           |${Prompting.itemsWithAlternativesResponse(noOfQuestions, n)}
           |""".stripMargin
      case ExpectedMultipleChoiceQuestion(n) =>
        s"""Each questions should have a number of possible options as an answer, from which zero or multiple can be selected
           |${Prompting.itemsWithAlternativesResponse(noOfQuestions, n)}""".stripMargin
    }

    val prompt =
      s"""$message
         |
         |List out the questions (items) on one line each
         |${noOfQuestions.map(n => s"There should be a total of $n questions").getOrElse("")}
         |
         |$questionSpecification""".stripMargin

    val messages = appendToHistory(history, prompt)
    gptApiClient.chatCompletions(messages).map { response =>
      val lastMessage = latestMessage(response)
      val lines       = lastMessage.split("\n")
      val questions   = (1 to lines.length).toList.zip(lines).map { case (idx, line) =>
        questionType match {
          case ExpectedBooleanQuestion           => BooleanQuestionFromGpt(idx, line)
          case ExpectedTextQuestion              => TextQuestionFromGpt(idx, line)
          case ExpectedNumberQuestion            => NumberQuestionFromGpt(idx, line)
          case ExpectedSingleChoiceQuestion(_)   =>
            val parts = line.split(";").toList
            SingleChoiceQuestionFromGpt(idx, parts.head, parts.tail)
          case ExpectedMultipleChoiceQuestion(_) =>
            val parts = line.split(";").toList
            MultipleChoiceQuestionFromGpt(idx, parts.head, parts.tail)
        }
      }
      QuestionsFromGptResponse(questions, lastMessage, history :+ MessageExchange(prompt, lastMessage))
    }
  }

  def provideContext(
      context: String,
      message: String,
      history: List[MessageExchange] = List.empty
  ): F[SimpleChatResponse] = {
    val prompt =
      s"""I will now provide some information. $message
         |
         |$context""".stripMargin
    simpleChat(prompt, history)
  }

  private def yesNo(b: Boolean) = if b then "yes" else "no"

  private def describeAnswer(answer: AnswerToQuestionFromGpt) = {
    val (q, a) = answer match {
      case AnswerToBooleanQuestionFromGpt(question, answer)               => (question, yesNo(answer))
      case AnswerToTextQuestionFromQpt(question, answer)                  => (question, answer)
      case AnswerToNumberQuestionFromGpt(question, answer)                => (question, answer.toString)
      case AnswerToSingleChoiceQuestionFromGpt(question, index)           => (question, question.options(index))
      case AnswerToMultipleChoiceQuestionFromGpt(question, answerIndices) =>
        (question, answerIndices.map(_ + 1).mkString(", "))
    }
    s"""Question: ${q.question}
       |Answer: $a
       |""".stripMargin
  }

  private def appendToHistory(history: List[MessageExchange], prompt: String): List[Message] =
    history.flatMap { m => Message(User, m.message) :: Message(Assistant, m.reply) :: Nil } :+ Message(User, prompt)

  def submitAnswersToQuestionsFromGpt(
      message: String,
      answers: List[AnswerToQuestionFromGpt],
      history: List[MessageExchange],
      format: ExpectedFormat = TextFormat
  ): F[ChatResponse] = {
    val describedAnswers = answers.map(describeAnswer).mkString("\n")

    val formatDescription = format match
      case TextFormat            => ""
      case ListFormat(noOfItems) => Prompting.specifyListFormat(noOfItems)
      case BooleanFormat         => Prompting.specifyBooleanFormat
      case NumberFormat          => Prompting.specifyNumberFormat
      case TableFormat(columns)  => Prompting.specifyTableFormat(columns)
    end formatDescription

    val prompt = format match
      case TextFormat =>
        s"""$describedAnswers
           |
           |$message""".stripMargin
      case _          =>
        s"""$describedAnswers
           |
           |$message
           |
           |$formatDescription""".stripMargin
    end prompt

    val messages = appendToHistory(history, prompt)
    gptApiClient.chatCompletions(messages).map { response =>
      val reply  = latestMessage(response)
      val parsed = parseMessage(reply, format)
      ChatResponse(parsed, reply, history :+ MessageExchange(prompt, reply))
    }
  }

  private def parseTable(message: String, columns: List[Column]): Option[Table] = {
    val lines = message.split("\n").toList.filter(_.contains(";"))
    val rows  = lines.map { line =>
      val parts = line.split(";")
      val cells = columns.indices.zip(columns).toList.map { case (index, column) =>
        val part: String       = parts(index)
        val cell: Option[Cell] = column match
          case BooleanColumn(_)               => parseBoolean(part).map(p => BooleanCell(p, column))
          case NumberColumn(_)                => parseNumber(part).map(n => NumberCell(n, column))
          case TextColumn(_)                  => Some(TextCell(part, column))
          case SingleChoiceColumn(_, options) =>
            parseSingleChoice(part, options).map(s => SingleChoiceCell(s, column))
        end cell
        cell
      }
      if cells.forall(_.isDefined) then Some(Row(cells.flatMap(_.toList))) else None
    }

    if rows.forall(_.isDefined) then Some(Table(columns, rows.flatMap(_.toList))) else None
  }

  private def parseMessage(message: String, format: ExpectedFormat): Data = format match {
    case TextFormat           => TextData(message)
    case ListFormat(_)        => ListData(parseList(message))
    case BooleanFormat        => parseBoolean(message).map(b => BooleanData(b)).getOrElse { ParseError }
    case NumberFormat         =>
      parseNumber(message) match
        case Some(value) => NumberData(value)
        case None        => ParseError
      end match
    case TableFormat(columns) =>
      val table = parseTable(message, columns)
      table match
        case Some(t) => TableData(t)
        case None    => ParseError
  }

  private def latestMessage(response: CompletionResponse) = response.choices.last.message.content

  private def parseSingleChoice(s: String, options: List[String]): Option[String] = options.find(_ === s)

  private def parseBoolean(s: String): Option[Boolean] = s.toLowerCase match {
    case "false" | "no" | "n" | "0" => Some(false)
    case "true" | "yes" | "y" | "1" => Some(true)
    case _                          => None
  }

  private def parseNumber(s: String): Option[Double] = Try(s.strip().toDouble).toOption

  private def parseList(s: String): List[String] = s.split("\n").toList

}
object Interactions {

  object Model {

    enum ExpectedQuestion:
      case ExpectedBooleanQuestion
      case ExpectedTextQuestion
      case ExpectedNumberQuestion
      case ExpectedSingleChoiceQuestion(noOfOptions: Option[Int])
      case ExpectedMultipleChoiceQuestion(noOfOptions: Option[Int])

    enum QuestionFromGpt(val index: Int, val question: String):
      case BooleanQuestionFromGpt(override val index: Int, override val question: String)
          extends QuestionFromGpt(index, question)
      case TextQuestionFromGpt(override val index: Int, override val question: String)
          extends QuestionFromGpt(index, question)
      case NumberQuestionFromGpt(override val index: Int, override val question: String)
          extends QuestionFromGpt(index, question)
      case SingleChoiceQuestionFromGpt(override val index: Int, override val question: String, options: List[String])
          extends QuestionFromGpt(index, question)
      case MultipleChoiceQuestionFromGpt(override val index: Int, override val question: String, options: List[String])
          extends QuestionFromGpt(index, question)

    enum AnswerToQuestionFromGpt:
      case AnswerToBooleanQuestionFromGpt(question: BooleanQuestionFromGpt, answer: Boolean)
      case AnswerToTextQuestionFromQpt(question: TextQuestionFromGpt, answer: String)
      case AnswerToNumberQuestionFromGpt(question: NumberQuestionFromGpt, answer: Double)
      case AnswerToSingleChoiceQuestionFromGpt(question: SingleChoiceQuestionFromGpt, answerIndex: Int)
      case AnswerToMultipleChoiceQuestionFromGpt(question: MultipleChoiceQuestionFromGpt, answerIndices: Set[Int])

    enum ExpectedFormat:
      case TextFormat
      case ListFormat(noOfItems: Option[Int])
      case BooleanFormat
      case NumberFormat
      case TableFormat(columns: List[Column])

    enum Data:
      case TextData(value: String)
      case ListData(items: List[String])
      case BooleanData(value: Boolean)
      case NumberData(value: Double)
      case TableData(table: Table)
      case ParseError

    case class MessageExchange(
        message: String,
        reply: String
    )

    case class SimpleChatResponse(
        message: String,
        history: List[MessageExchange]
    )

    case class ChatResponse(
        data: Data,
        rawMessage: String,
        history: List[MessageExchange]
    )

    case class QuestionsFromGptResponse(
        questions: List[QuestionFromGpt],
        rawMessage: String,
        history: List[MessageExchange]
    )

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

    case class Table(
        columns: List[Column],
        rows: List[Row]
    )

    case class Row(
        columns: List[Cell]
    )

  }

}
