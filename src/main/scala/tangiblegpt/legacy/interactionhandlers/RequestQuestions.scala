package tangiblegpt.legacy.interactionhandlers

import RequestQuestions.QuestionType
import RequestQuestions.QuestionType.{
  MultipleChoiceQuestions,
  SingleChoiceQuestions,
  TextQuestions,
  YesNoQuestions
}
import RequestQuestions.QuestionFromGpt.{
  BooleanQuestionFromGpt,
  MultipleChoiceQuestionFromGpt,
  SingleChoiceQuestionFromGpt,
  TextQuestionFromGpt
}
import tangiblegpt.legacy.model.InteractionHandler

object RequestQuestions:

  case class QuestionExpectingFollowupQuestions(
      question: String,
      expectedQuestionType: QuestionType,
      noOfQuestions: Option[Int]
  )

  enum Difficulty:
    case Easy, Medium, Hard

  case class QuizRequest(
      topic: String,
      difficulty: Difficulty,
      expectedQuestionType: QuestionType,
      noOfQuestions: Option[Int]
  )

  enum QuestionFromGpt(val index: Int, val question: String):
    case BooleanQuestionFromGpt(override val index: Int, override val question: String)
        extends QuestionFromGpt(index, question)
    case TextQuestionFromGpt(override val index: Int, override val question: String)
        extends QuestionFromGpt(index, question)
    case SingleChoiceQuestionFromGpt(override val index: Int, override val question: String, options: List[String])
        extends QuestionFromGpt(index, question)
    case MultipleChoiceQuestionFromGpt(override val index: Int, override val question: String, options: List[String])
        extends QuestionFromGpt(index, question)

  enum QuestionType:
    case YesNoQuestions
    case TextQuestions
    case SingleChoiceQuestions(noOfOptions: Option[Int])
    case MultipleChoiceQuestions(noOfOptions: Option[Int])

  val requestFollowupQuestionsHandler: InteractionHandler[QuestionExpectingFollowupQuestions, List[QuestionFromGpt]] =
    new InteractionHandler[QuestionExpectingFollowupQuestions, List[QuestionFromGpt]](
      q =>
        q.noOfQuestions match
          case Some(n) => s"There should be $n questions in total."
          case None    => ""
      ,
      q => responseFormatPrompt(q.expectedQuestionType, q.noOfQuestions),
      (q, s) => parseQuestions(q.expectedQuestionType, s),
      objective = Some("Ask me followup questions to be able to give me a better answer.")
    )

  val requestQuizQuestions: InteractionHandler[QuizRequest, List[QuestionFromGpt]] =
    new InteractionHandler[QuizRequest, List[QuestionFromGpt]](
      q => s"""Topic: ${q.topic}
           |Difficulty: ${q.difficulty.toString}
           |${q.noOfQuestions.map(n => s"Number of questions: $n").getOrElse("")}""".stripMargin,
      q => responseFormatPrompt(q.expectedQuestionType, q.noOfQuestions),
      (q, s) => parseQuestions(q.expectedQuestionType, s),
      objective = Some(s"I want you to be a quiz master and ask me questions.")
    )

  private def parseQuestions(questionType: QuestionType, s: String): Option[List[QuestionFromGpt]] =
    val lines     = s.split("\n")
    val questions = (1 to lines.length).toList.zip(lines).map { case (idx, line) =>
      questionType match {
        case YesNoQuestions             => BooleanQuestionFromGpt(idx, line)
        case TextQuestions              => TextQuestionFromGpt(idx, line)
        case SingleChoiceQuestions(_)   =>
          val parts = line.split(";").toList
          SingleChoiceQuestionFromGpt(idx, parts.head, parts.tail)
        case MultipleChoiceQuestions(_) =>
          val parts = line.split(";").toList
          MultipleChoiceQuestionFromGpt(idx, parts.head, parts.tail)
      }
    }
    Some(questions) // TODO None if parse error

  private def responseFormatPrompt(questionType: QuestionType, noOfQuestions: Option[Int]) =
    val describePossibleAnswers = questionType match {
      case YesNoQuestions                       => s"""All questions should only have "yes" or "no" answers."""
      case TextQuestions                        => ""
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
