package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import xf.examples.Common.{clientResource, createTangibleClient, extractKey, msg}
import xf.Input.prompt

object SimpleQuery extends IOApp:

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val tc = createTangibleClient(client, extractKey())
      for
        prompt <- prompt("Prompt")
        answer <- tc.plainTextChat(msg(prompt))
        _      <- Console[IO].println(answer.message)
      yield ExitCode.Success
    }
