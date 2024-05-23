# tangible-gpt

Make LLM integration more tangible through type safety

## Motivation

Strict typing can make APIs easier to work with and harder to use wrongly. LLMs, like
OpenAI's GPT4, are pretty good at responding in a specific syntax such as JSON.

tangible-gpt simplifies typed, structured interactions with OpenAI's chat completion
API (with intention of supporting others). It also supports simplified function calling
and automating different reasoning strategies
(see https://cookbook.openai.com/articles/techniques_to_improve_reliability)

## Examples

### Json response
```scala 3
case class Person(name: String, nationality: String, age: Int)
given Codec[Person] = deriveCodec
val example = Person("Jose", "Spain", 52)

tc.expectJson("Give me 10 random people", List(example)).map { 
  (response: Either[FailedInteraction, TangibleResponse[List[Person]]]) => ??? 
}
```

### Boolean response
```scala 3
tc.expectJson("Is AI smarter most than humans?").amp { 
  (response: Either[FailedInteraction, TangibleResponse[Boolean]]) => ??? 
}
```

### Double response
```scala 3
tc.expectDouble("Approximately how many people live in Norway?").map { 
  (response: Either[FailedInteraction, TangibleResponse[Double]]) => ???
}
```

### Plain text
```scala 3
tc.expectPlainText("How are you?").map { 
  (response: TangibleResponse[String]) => ???
}
```

### Optional response
```scala 3
tc.expectDoubleOption("What is the meaning of life?").map {
  (response: Either[FailedInteraction, TangibleOptionResponse[Double]]) => ???
}
```

### Function calling
```scala 3
def sum(a: Int, b: Int) = a + b

def f(s: String): IO[String] =
  case class SumParams(a: Int, b: Int)
  object SumParams:
    given Decoder[SumParams] = Decoder { c =>
      for
        a <- c.downField("a").as[Int]
        b <- c.downField("b").as[Int]
      yield SumParams(a, b)
    }
  val params: SumParams = decode[SumParams](s)(using summon[Decoder[SumParams]]).toOption.get
  sum(params.a, params.b).toString.pure[IO]

val fc = FunctionCall(
  "sum_of_ints",
  "Sum of two ints",
  List(IntegerParam("a", "a"), IntegerParam("b", "b")),
  s => f(s)
)

tc.expectDouble(
  "What is What is 87878 + 23255?",
  functionCalls = List(fc)
).map {
  (response: Either[FailedInteraction, TangibleResponse[Double]]) => ???
}
```

### Custom reasoning strategy

### Using EitherT

Also, have a look in the `examples` folder for more examples of usage
