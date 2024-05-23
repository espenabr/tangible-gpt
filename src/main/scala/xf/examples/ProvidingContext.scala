package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import xf.Input.{prompt, readFileContent}
import xf.examples.Common.{clientResource, createTangibleClient, extractKey, msg}

object ProvidingContext extends IOApp:

  /*
   * Local files can be sent as context and utilized later in the conversation
   */

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val tc = createTangibleClient(client, extractKey(args))
      for {
        context  <- readFileContent("README.md")
        reply    <- tc.plainTextChat(msg(s"""I will provide some information relevant for later.
                                            |$context"""))
        prompt   <- prompt("Ask questions about README.md")
        response <- tc.plainTextChat(msg(prompt), reply.history)
        _        <- Console[IO].println(response.message)
        _        <- Chat.chat(tc, response.history) // continue chat
      } yield ExitCode.Success
    }
