package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*
import io.circe.*
 import xf.examples.Common.{clientResource, createTangibleClient, extractKey}

object JsonResponse extends IOApp:

  case class Person(name: String, nationality: String, age: Int)
  given Codec[Person] = deriveCodec

  override def run(args: List[String]): IO[ExitCode] =
    clientResource
      .use { client =>
        val tc      = createTangibleClient(client, extractKey())
        val example = Person("Jose", "Spain", 52)

        for
          response <- tc.expectJson("Give me 10 random people", List(example))
          _        <- response.map { r => IO.println(r.value) }.getOrElse( IO.raiseError(new RuntimeException()) )
        yield ExitCode.Success
      }