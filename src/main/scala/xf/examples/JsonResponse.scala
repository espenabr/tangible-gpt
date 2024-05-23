package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*
import io.circe.*
import io.circe.parser.decode
import xf.examples.Common.{clientResource, tangibleClient, extractKey}

object JsonResponse extends IOApp:

  case class Person(name: String, nationality: String, age: Int)
  given Encoder[Person] = deriveEncoder
  given Decoder[Person] = deriveDecoder

  override def run(args: List[String]): IO[ExitCode] =
    clientResource
      .use { client =>
        val tc      = tangibleClient(client, extractKey(args))
        val example = Person("Jose", "Spain", 52)

        for
          response <- tc.expectJson("Give me 10 random people", List(example))
          _        <- response.map { r => IO.println(r.value) }.getOrElse( IO.raiseError(new RuntimeException()) )
        yield ExitCode.Success
      }