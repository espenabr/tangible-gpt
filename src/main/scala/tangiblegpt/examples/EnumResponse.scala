package tangiblegpt.examples

import cats.effect.{IO, IOApp}
import io.circe.generic.semiauto.*
import io.circe.*
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}
import cats.effect.std.Console

object EnumResponse extends IOApp.Simple:

  enum Color:
    case Red, Blue, Black, White

  val run: IO[Unit] =
    clientResource.use { client =>
      val tc = createTangibleClient(client, extractKey())

      for
        response <- tc.expectEnumCase("What is the most common color of a swan?", Color.values.toList)
        _        <- response.map { r => Console[IO].println(r.value) }.getOrElse(IO.raiseError(new RuntimeException()))
      yield ()
    }
