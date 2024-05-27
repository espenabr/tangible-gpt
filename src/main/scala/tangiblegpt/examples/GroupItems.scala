package tangiblegpt.examples

import cats.effect.{IO, IOApp}
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}
import cats.effect.std.Console

object GroupItems extends IOApp.Simple:

  override def run: IO[Unit] =
    clientResource.use { client =>
      val tc    = createTangibleClient(client, extractKey())
      val items = List("cat", "tuna", "dog", "parrot", "mouse", "owl", "eel")

      for
        response <- tc.expectGroups(items, None, Some("Type of animal"))
        _        <- response.map { r => Console[IO].println(r.value) }.getOrElse { IO.raiseError(new RuntimeException()) }
      yield ()
    }
