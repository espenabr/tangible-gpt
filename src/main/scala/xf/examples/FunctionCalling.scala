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

      for
        response <- tc.expectDouble(
          "What is What is 87878 + 23255?",
          functionCalls = List(sumFunctionCall)
        )
        _ <- IO.println(response.toOption.map(_.value).getOrElse("-"))
      yield ExitCode.Success
    }
