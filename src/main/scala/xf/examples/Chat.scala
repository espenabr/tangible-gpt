package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import xf.TangibleClient
import xf.examples.Common.{clientResource, createTangibleClient, extractKey, msg}
import xf.Input.prompt
import xf.gpt.GptApiClient.Common.Message

object Chat extends IOApp:

  /*
   * Continuous command-line chat
   */

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client => chat(createTangibleClient(client, extractKey()), List.empty) }

  def chat(tc: TangibleClient[IO], history: List[Message]): IO[ExitCode] =
    for
      message <- prompt("Chat")
      reply   <- tc.plainTextChat(msg(message), history)
      _       <- Console[IO].println(reply.message)
      _       <- chat(tc, reply.history)
    yield ExitCode.Success
