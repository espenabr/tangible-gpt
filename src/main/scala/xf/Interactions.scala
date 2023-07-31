package xf

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
import xf.model.{ChatResponse, RequestHandler, ResponseHandler}
import xf.Interactions.Model.ExpectedQuestion.{
  ExpectedBooleanQuestion,
  ExpectedMultipleChoiceQuestion,
  ExpectedNumberQuestion,
  ExpectedSingleChoiceQuestion,
  ExpectedTextQuestion
}
import xf.Interactions.Model.{
  AnswerToQuestionFromGpt,
  ExpectedQuestion,
  MessageExchange,
  QuestionFromGpt,
  QuestionsFromGptResponse,
  SimpleChatResponse
}
import xf.Interactions.Model.AnswerToQuestionFromGpt.{
  AnswerToBooleanQuestionFromGpt,
  AnswerToMultipleChoiceQuestionFromGpt,
  AnswerToNumberQuestionFromGpt,
  AnswerToSingleChoiceQuestionFromGpt,
  AnswerToTextQuestionFromQpt
}
import xf.ResponseHandlers.{booleanResponseHandler, doubleResponseHandler, listResponseHandler, tableResponseHandler}

class Interactions[F[_]: Concurrent](gptApiClient: GptApiClient[F]) {

  val RequestValuePlaceholder = "{{value}}"

  def simpleChat(message: String, history: List[MessageExchange] = List.empty): F[SimpleChatResponse] = {
    val messages = appendToHistory(history, message)
    gptApiClient.chatCompletions(messages).map { response =>
      val reply = latestMessage(response)
      SimpleChatResponse(reply, history :+ MessageExchange(message, reply))
    }
  }

  def chat[A](
      message: String,
      handler: ResponseHandler[A],
      history: List[MessageExchange] = List.empty
  ): F[ChatResponse[A]] = {
    val prompt =
      s"""$message
         |
         |${handler.typePrompt}""".stripMargin

    simpleChat(prompt, history).map { response =>
      ChatResponse(
        handler.parse(response.message),
        response.message,
        history :+ MessageExchange(prompt, response.message)
      )
    }
  }

  def templatedChat[A, B](
      message: String,
      requestValue: A,
      requestHandler: RequestHandler[A],
      responseHandler: ResponseHandler[B],
      history: List[MessageExchange] = List.empty
  ): F[ChatResponse[B]] = {
    val prompt: String =
      if message.contains(RequestValuePlaceholder) then
        message.replaceAll(RequestValuePlaceholder, requestHandler.serialize(requestValue))
      else s"""$message
           |
           |Input:
           |${requestHandler.serialize(requestValue)}""".stripMargin

    chat(prompt, responseHandler, history)
  }

  def chatExpectingNumber(message: String, history: List[MessageExchange] = List.empty): F[ChatResponse[Double]] =
    chat(message, doubleResponseHandler, history)

  def chatExpectingTable(
      message: String,
      columns: List[model.Table.Column],
      history: List[MessageExchange] = List.empty
  ): F[ChatResponse[model.Table]] =
    chat(message, tableResponseHandler(columns), history)

  def chatExpectingBoolean(message: String, history: List[MessageExchange] = List.empty): F[ChatResponse[Boolean]] =
    chat(message, booleanResponseHandler, history)

  def chatExpectingList(
      message: String,
      noOfOptions: Option[Int],
      history: List[MessageExchange] = List.empty
  ): F[ChatResponse[List[String]]] =
    chat(message, listResponseHandler(noOfOptions), history)

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

  def submitAnswersToQuestionsFromGpt[A](
      message: String,
      answers: List[AnswerToQuestionFromGpt],
      history: List[MessageExchange],
      handler: ResponseHandler[A]
  ): F[ChatResponse[A]] = {
    val describedAnswers = answers.map(describeAnswer).mkString("\n")

    val prompt =
      s"""$describedAnswers
           |
           |$message
           |
           |${handler.typePrompt}""".stripMargin

    val messages = appendToHistory(history, prompt)
    gptApiClient.chatCompletions(messages).map { response =>
      val reply = latestMessage(response)
      ChatResponse[A](handler.parse(reply), reply, history :+ MessageExchange(prompt, reply))
    }
  }

  private def latestMessage(response: CompletionResponse) = response.choices.last.message.content

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

    case class MessageExchange(
        message: String,
        reply: String
    )

    case class SimpleChatResponse(
        message: String,
        history: List[MessageExchange]
    )

    case class QuestionsFromGptResponse(
        questions: List[QuestionFromGpt],
        rawMessage: String,
        history: List[MessageExchange]
    )

  }

}
