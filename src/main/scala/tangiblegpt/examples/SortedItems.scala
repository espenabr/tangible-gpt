package tangiblegpt.examples

import cats.effect.{IO, IOApp}
import cats.effect.std.Console
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}

object SortedItems extends IOApp.Simple:

  override def run: IO[Unit] =
    clientResource.use { client =>
      val tc = createTangibleClient(client, extractKey())

      val items = List("debug", "warn", "fatal", "trace", "info", "error")

      for
        response <- tc.expectSorted(items, Some("log level severity"))
        _        <- response.map(r => Console[IO].println(r.value)).getOrElse(IO.raiseError(new RuntimeException()))
      yield ()
    }
