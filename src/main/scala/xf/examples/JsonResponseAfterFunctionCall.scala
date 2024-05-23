package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.generic.semiauto.*
import io.circe.*
import io.circe.parser.decode
import xf.examples.Common.{clientResource, createTangibleClient, extractKey}
import xf.model.{FunctionCall, Param, TangibleResponse}
import xf.model.Param.IntegerParam
import cats.implicits.*
import xf.model.ReasoningStrategy.ThinkStepByStep


object JsonResponseAfterFunctionCall extends IOApp:

  case class Result(result: Int)
  given Codec[Result] = deriveCodec

  override def run(args: List[String]): IO[ExitCode] =
    clientResource
      .use { client =>
        val tc      = createTangibleClient(client, extractKey(args))
        val example = Result(1234)

        def concatenateNumbers(a: Int, b: Int) = (a.toString + b.toString).toInt

        def f(s: String): IO[String] =
          case class Params(a: Int, b: Int)
          given Decoder[Params] = deriveDecoder

          val params: Params = decode[Params](s)(using summon[Decoder[Params]]).toOption.get
          concatenateNumbers(params.a, params.b).toString.pure[IO]

        for
          response <- tc.expectJson(
            s"""Calculate this expression using the |+| operator
               |
               |473 |+| 745 = """.stripMargin,
            example,
            functionCalls = List(
              FunctionCall(
                "concatenate_numbers",
                "Concatenate numbers using the |+| operator",
                List(IntegerParam("a", "a"), IntegerParam("b", "b")),
                f
              )
            ),
            reasoningStrategy = ThinkStepByStep
          )
          _        <- response.map { r => IO.println(r.value) }.getOrElse( IO.raiseError(new RuntimeException()) )
        yield ExitCode.Success
      }
