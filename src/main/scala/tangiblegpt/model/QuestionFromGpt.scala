package tangiblegpt.model

enum QuestionFromGpt(val index: Int, val question: String):
  case BooleanQuestionFromGpt(override val index: Int, override val question: String)
    extends QuestionFromGpt(index, question)
  case TextQuestionFromGpt(override val index: Int, override val question: String)
    extends QuestionFromGpt(index, question)
  case SingleChoiceQuestionFromGpt(override val index: Int, override val question: String, options: List[String])
    extends QuestionFromGpt(index, question)
  case MultipleChoiceQuestionFromGpt(override val index: Int, override val question: String, options: List[String])
    extends QuestionFromGpt(index, question)
