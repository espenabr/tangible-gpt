package tangiblegpt.model

import tangiblegpt.model.QuestionFromGpt.{
  BooleanQuestionFromGpt,
  MultipleChoiceQuestionFromGpt,
  SingleChoiceQuestionFromGpt,
  TextQuestionFromGpt
}

enum AnswerToQuestionFromGpt:
  case AnswerToBooleanQuestionFromGpt(question: BooleanQuestionFromGpt, answer: Boolean)
  case AnswerToTextQuestionFromQpt(question: TextQuestionFromGpt, answer: String)
  case AnswerToSingleChoiceQuestionFromGpt(question: SingleChoiceQuestionFromGpt, answerIndex: Int)
  case AnswerToMultipleChoiceQuestionFromGpt(
      question: MultipleChoiceQuestionFromGpt,
      answerIndices: Set[Int]
  )
