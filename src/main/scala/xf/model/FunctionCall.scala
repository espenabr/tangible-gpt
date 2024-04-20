package xf.model

import cats.Monad

class FunctionCall[F[_]: Monad](
    val name: String,
    val description: String,
    val params: List[Param],
    val function: String => F[String]
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

enum ReasoningStrategy:
  case None, ThinkStepByStep, SuggestMultipleAndPickOne