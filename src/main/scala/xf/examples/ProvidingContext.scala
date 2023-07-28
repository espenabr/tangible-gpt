package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import xf.Input.{prompt, readFileContent}
import xf.examples.Common.{clientResource, createConversationClient, extractKey}
import xf.examples.Chat

object ProvidingContext extends IOApp {

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val interactions = createConversationClient(client, extractKey(args))
      for {
        context  <- readFileContent("README.md")
        reply    <- interactions.provideContext(context, "Answer questions about it")
        prompt   <- prompt("Ask questions about README.md")
        response <- interactions.simpleChat(prompt, reply.history)
        _        <- Console[IO].println(response.message)
        _        <- Chat.chat(interactions, response.history) // continue chat
      } yield ExitCode.Success
    }

}
