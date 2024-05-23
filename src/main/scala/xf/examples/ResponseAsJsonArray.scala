package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import xf.examples.Common.{clientResource, tangibleClient, extractKey}
import xf.interactionhandlers.RequestJsonArray
import io.circe.generic.semiauto.*
import io.circe.*

object ResponseAsJsonArray extends IOApp:

  case class Person(name: String, nationality: String, age: Int)
  given Encoder[Person] = deriveEncoder
  given Decoder[Person] = deriveDecoder

  override def run(args: List[String]): IO[ExitCode] =
    clientResource
      .use { client =>
        val ic      = tangibleClient(client, extractKey(args))
        val example = Person("Jose", "Spain", 52)
        val handler = RequestJsonArray.createHandler(example)

        for
          response <- ic.chat("Give me a list of random people", handler)
          _        <- IO.println(response.value.get)
        yield ExitCode.Success
      }
