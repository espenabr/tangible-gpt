package tangiblegpt.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}
import tangiblegpt.Input.prompt

object SimpleQuery extends IOApp.Simple:

  val run: IO[Unit] = clientResource
    .use { client =>
      val tc = createTangibleClient(client, extractKey())
      for
        prompt <- prompt("Prompt")
        answer <- tc.expectPlainText(prompt)
        _      <- Console[IO].println(answer.value)
      yield ExitCode.Success
    }
