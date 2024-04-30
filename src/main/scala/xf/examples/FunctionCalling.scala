package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.Decoder
import xf.examples.Common.{clientResource, createInteractionClient, extractKey}
import xf.model.Param.IntegerParam
import xf.model.{FunctionCall, InteractionHandler, Param}
import io.circe._, io.circe.parser._
import cats.implicits.*

object FunctionCalling extends IOApp:

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val ic = createInteractionClient(client, extractKey(args))

      val handler = new InteractionHandler[String, String](s => s, s => s, (a, b) => Some(b))

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

      for
        aa <- ic.chat(
                "What is 87878 + 23255?",
                handler,
                List(fc)
              )
        _  <- IO.println(aa.rawMessage)
      yield ExitCode.Success
    }
