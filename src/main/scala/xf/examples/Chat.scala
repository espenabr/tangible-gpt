package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import org.http4s.ember.client.EmberClientBuilder
import xf.Interactions
import xf.Interactions.Model.{MessageExchange, SimpleChatResponse}
import xf.gpt.GptApiClient
import xf.examples.Common.{clientResource, createConversationClient, extractKey}
import xf.Input.prompt

object Chat extends IOApp {

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client => chat(createConversationClient(client, extractKey(args)), List.empty) }

  def chat(interactions: Interactions[IO], history: List[MessageExchange]): IO[ExitCode] =
    for {
      message <- prompt("Chat")
      reply   <- interactions.simpleChat(message, history)
      _       <- Console[IO].println(reply.message)
      _       <- chat(interactions, reply.history)
    } yield ExitCode.Success

}
