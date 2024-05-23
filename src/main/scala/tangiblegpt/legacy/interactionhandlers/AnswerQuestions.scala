package tangiblegpt.legacy.interactionhandlers

import AnswerQuestions.AnswerToQuestionFromGpt.{
  AnswerToBooleanQuestionFromGpt,
  AnswerToMultipleChoiceQuestionFromGpt,
  AnswerToSingleChoiceQuestionFromGpt,
  AnswerToTextQuestionFromQpt
}
import RequestQuestions.QuestionFromGpt.{
  BooleanQuestionFromGpt,
  MultipleChoiceQuestionFromGpt,
  SingleChoiceQuestionFromGpt,
  TextQuestionFromGpt
}
import tangiblegpt.legacy.model.InteractionHandler

object AnswerQuestions:

  enum AnswerToQuestionFromGpt:
    case AnswerToBooleanQuestionFromGpt(question: BooleanQuestionFromGpt, answer: Boolean)
    case AnswerToTextQuestionFromQpt(question: TextQuestionFromGpt, answer: String)
    case AnswerToSingleChoiceQuestionFromGpt(question: SingleChoiceQuestionFromGpt, answerIndex: Int)
    case AnswerToMultipleChoiceQuestionFromGpt(question: MultipleChoiceQuestionFromGpt, answerIndices: Set[Int])

  val answerQuestionsHandler = new InteractionHandler[List[AnswerToQuestionFromGpt], String](
    renderAnswers,
    _ => "",
    (_, s) => Some(s),
    objective = Some("Here are the answers to the questions")
  )

  private def renderAnswers(answers: List[AnswerToQuestionFromGpt]) =
    answers.map(describeAnswer).mkString("\n")

  private def describeAnswer(answer: AnswerToQuestionFromGpt) = {
    val (q, a) = answer match
      case AnswerToBooleanQuestionFromGpt(question, answer)               => (question, yesNo(answer))
      case AnswerToTextQuestionFromQpt(question, answer)                  => (question, answer)
      case AnswerToSingleChoiceQuestionFromGpt(question, index)           => (question, question.options(index))
      case AnswerToMultipleChoiceQuestionFromGpt(question, answerIndices) =>
        (question, answerIndices.map(_ + 1).mkString(", "))

    s"""Question: ${q.question}
       |Answer: $a
       |""".stripMargin
  }

  private def yesNo(b: Boolean) = if b then "yes" else "no"