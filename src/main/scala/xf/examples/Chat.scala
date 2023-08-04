package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import org.http4s.ember.client.EmberClientBuilder
import xf.InteractionClient
import xf.model.{MessageExchange, SimpleChatResponse}
import xf.gpt.GptApiClient
import xf.examples.Common.{clientResource, createInteractionClient, extractKey}
import xf.Input.prompt

object Chat extends IOApp {

  /*
   * Continuous command-line chat
   */

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client => chat(createInteractionClient(client, extractKey(args)), List.empty) }

  def chat(ic: InteractionClient[IO], history: List[MessageExchange]): IO[ExitCode] =
    for {
      message <- prompt("Chat")
      reply   <- ic.plainTextChat(message, history)
      _       <- Console[IO].println(reply.message)
      _       <- chat(ic, reply.history)
    } yield ExitCode.Success

}
