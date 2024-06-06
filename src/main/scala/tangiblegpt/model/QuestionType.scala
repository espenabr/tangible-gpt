package tangiblegpt.model

enum QuestionType:
  case YesNoQuestions
  case TextQuestions
  case SingleChoiceQuestions(noOfOptions: Option[Int])
  case MultipleChoiceQuestions(noOfOptions: Option[Int])
