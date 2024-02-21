package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import xf.InteractionClient
import xf.examples.Common.{clientResource, createInteractionClient, extractKey, msg}
import xf.Input.prompt
import xf.gpt.GptApiClient.Common.Message

object Chat extends IOApp {

  /*
   * Continuous command-line chat
   */

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client => chat(createInteractionClient(client, extractKey(args)), List.empty) }

  def chat(ic: InteractionClient[IO], history: List[Message]): IO[ExitCode] =
    for {
      message <- prompt("Chat")
      reply   <- ic.plainTextChat(msg(message), history)
      _       <- Console[IO].println(reply.message)
      _       <- chat(ic, reply.history)
    } yield ExitCode.Success

}
