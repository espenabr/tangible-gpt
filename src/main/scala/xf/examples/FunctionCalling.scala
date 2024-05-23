package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.Decoder
import xf.examples.Common.{clientResource, createTangibleClient, extractKey}
import xf.model.Param.IntegerParam
import xf.model.{FunctionCall, Param}
import io.circe.*
import io.circe.parser.*
import cats.implicits.*
import io.circe.generic.semiauto.deriveDecoder

object FunctionCalling extends IOApp:

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val tc = createTangibleClient(client, extractKey(args))

      def sum(a: Int, b: Int) = a + b

      def sumWrapper(s: String): IO[String] =
        case class SumParams(a: Int, b: Int)
        given Decoder[SumParams] = deriveDecoder
        for
          params <- IO.fromEither(decode[SumParams](s))
        yield sum(params.a, params.b).toString

      val fc = FunctionCall(
        "sum_of_ints",
        "Sum of two ints",
        List(IntegerParam("a", "a"), IntegerParam("b", "b")),
        s => sumWrapper(s)
      )

      for
        response <- tc.expectDouble(
          "What is What is 87878 + 23255?",
          functionCalls = List(fc)
        )
        _ <- IO.println(response.toOption.map(_.value).getOrElse("-"))
      yield ExitCode.Success
    }
