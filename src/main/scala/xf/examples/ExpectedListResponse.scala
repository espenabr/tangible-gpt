package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import xf.Interactions
import xf.examples.Common.{clientResource, createConversationClient, extractKey}
import xf.Input.prompt
import xf.Interactions.Model.ExpectedFormat.ListFormat

object ExpectedListResponse extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    clientResource
      .use { client =>
        val interactions = createConversationClient(client, extractKey(args))
        for {
          input  <- prompt("Ask for something listable")
          answer <- interactions.chat(input, ListFormat(None))
          _      <- Console[IO].println(answer)
        } yield ExitCode.Success
      }

}
