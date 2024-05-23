package tangiblegpt.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import tangiblegpt.TangibleClient
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey, msg}
import tangiblegpt.Input.prompt
import tangiblegpt.gpt.GptApiClient.Common.Message

object Chat extends IOApp:

  /*
   * Continuous command-line chat
   */

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client => chat(createTangibleClient(client, extractKey()), List.empty) }

  def chat(tc: TangibleClient[IO], history: List[Message]): IO[ExitCode] =
    for
      message <- prompt("Chat")
      reply   <- tc.expectPlainText(message, history)
      _       <- Console[IO].println(reply.value)
      _       <- chat(tc, reply.history)
    yield ExitCode.Success
