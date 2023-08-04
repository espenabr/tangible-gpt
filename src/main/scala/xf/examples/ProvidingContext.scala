package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import xf.Input.{prompt, readFileContent}
import xf.examples.Common.{clientResource, createInteractionClient, extractKey}
import xf.examples.Chat

object ProvidingContext extends IOApp {

  /*
   * Local files can be sent as context and utilized later in the conversation
   */

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val ic = createInteractionClient(client, extractKey(args))
      for {
        context  <- readFileContent("README.md")
        reply    <- ic.plainTextChat(s"""I will provide some information relevant for later.
                                            |$context""")
        prompt   <- prompt("Ask questions about README.md")
        response <- ic.plainTextChat(prompt, reply.history)
        _        <- Console[IO].println(response.message)
        _        <- Chat.chat(ic, response.history) // continue chat
      } yield ExitCode.Success
    }

}
