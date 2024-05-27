package tangiblegpt.examples

import cats.effect.{IO, IOApp}
import cats.effect.std.Console
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}

object FilteredItems extends IOApp.Simple:

  override def run: IO[Unit] =
    clientResource.use { client =>
      val tc    = createTangibleClient(client, extractKey())
      val items = List("happy", "sad", "excited", "angry", "frustrated", "satisfied", "worried")

      for
        response <- tc.expectFiltered(items, "positive feelings")
        _        <- response.map(r => Console[IO].println(r.value)).getOrElse(IO.raiseError(new RuntimeException()))
      yield ()
    }
