package xf.model

case class FunctionCall(
    name: String,
    description: String,
    params: List[Param],
    function: String => String
)

enum Param:
  case IntegerParam(
      name: String,
      description: String
  )
  case StringParam(
      name: String,
      description: String
  )
  case EnumParam(
      name: String,
      description: String,
      _enum: List[String]
  )
