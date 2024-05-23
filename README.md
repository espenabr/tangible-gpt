# tangible-gpt

Make LLM integration more tangible through type safety

## Motivation

Strict typing can make APIs easier to work with and harder to misuse. LLMs, like
OpenAI's GPT4, are pretty good at responding in a specific syntax such as JSON.

tangible-gpt simplifies typed, structured interactions with OpenAI's chat completion
API (with intention of supporting others). It also simplifies function calling
and automating different reasoning strategies
(see https://cookbook.openai.com/articles/techniques_to_improve_reliability)

## Examples

### Json response
```scala 3
case class Person(name: String, nationality: String, age: Int)
given Codec[Person] = deriveCodec
val example = Person("Jose", "Spain", 52)

tangibleClient.expectJson("Give me 10 random people", List(example)).map { 
  (response: Either[FailedInteraction, TangibleResponse[List[Person]]]) => ??? 
}
```

### Boolean response
```scala 3
tangibleClient.expectJson("Is AI smarter most than humans?").amp { 
  (response: Either[FailedInteraction, TangibleResponse[Boolean]]) => ??? 
}
```

### Numeric response
```scala 3
tangibleClient.expectDouble("Approximately how many people live in Norway?").map { 
  (response: Either[FailedInteraction, TangibleResponse[Double]]) => ???
}
```

### Plain text response
```scala 3
tangibleClient.expectPlainText("How are you?").map { 
  (response: TangibleResponse[String]) => ???
}
```

### Optional response
```scala 3
tangibleClient.expectDoubleOption("What is the meaning of life?").map {
  (response: Either[FailedInteraction, TangibleOptionResponse[Double]]) => ???
}
```

### With function calling
```scala 3
def sum(a: Int, b: Int): Int = a + b

val sumFunctionCall: FunctionCall[IO] =
  case class SumParams(a: Int, b: Int)
  given Decoder[SumParams] = deriveDecoder
  FunctionCall[IO](
    "sum_of_ints",
    "Sum of two ints",
    List(IntegerParam("a", "a"), IntegerParam("b", "b")),
    s => IO.fromEither(decode[SumParams](s)).map { params => sum(params.a, params.b).toString }
  )

tangibleClient.expectDouble(
  "What is What is 87878 + 23255?",
  functionCalls = List(sumFunctionCall)
).map {
  (response: Either[FailedInteraction, TangibleResponse[Double]]) => ???
}
```

### With Either type

```scala 3
tangibleClient.expectJsonEither(
  "How much does a fish weigh?",
  Answer("answer"),
  Clarifications(List("question", "question"))
).map { (response: Either[FailedInteraction, TangibleEitherResponse[Answer, Clarifications]]) =>
  ???
}
```

### Custom reasoning strategy
```scala 3
tangibleClient.expectDouble(
  "A juggler has 16 balls. Half of the balls are golf balls and half of the golf balls are blue. How many blue golf balls are there?",
  reasoningStrategy = ReasoningStrategy.ThinkStepByStep
).map { (response: Either[FailedInteraction, TangibleResponse[Double]]) =>
  ???
}
```

### Chained requests
```scala 3
case class ThingToDo(activity: String)
given Codec[ThingToDo] = deriveCodec

case class ActivityDetails(activity: String, description: String, difficulty1To10: Int)
given Codec[ActivityDetails] = deriveCodec

val result: IO[Either[FailedInteraction, ActivityDetails]] =
  (for
    response1 <- EitherT(tc.expectJson("I'm bored, give me some suggestions of things to do", List(ThingToDo("activity"))))
    response2 <- EitherT(
      tc.expectJson(
        s"Tell me more about ${Random.shuffle(response1.value).head.activity}",
        ActivityDetails("activity", "description", 5),
        history = response1.history
      )
    )
  yield response2.value).value
```

Also, have a look in the `examples` folder for more examples of usage
