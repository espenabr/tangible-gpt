package tangiblegpt.examples

import cats.effect.{IO, IOApp}
import cats.effect.std.Console
import tangiblegpt.examples.Common.{clientResource, extractKey}
import tangiblegpt.gpt.EmbeddingsClient

object CreateEmbeddings extends IOApp.Simple:

  override def run: IO[Unit] = clientResource
    .use { client =>
      val ec = EmbeddingsClient[IO](client, extractKey())

      for
        response <- ec.embeddings("Make me some embeddings of this meaningless text!")
        _        <- Console[IO].println(response)
      yield ()
    }
