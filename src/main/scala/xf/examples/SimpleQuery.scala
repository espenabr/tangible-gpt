package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import xf.examples.Common.{clientResource, createInteractionClient, extractKey}
import xf.Input.prompt

object SimpleQuery extends IOApp {

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val ic = createInteractionClient(client, extractKey(args))
      for {
        prompt <- prompt("Prompt")
        answer <- ic.plainTextChat(prompt)
        _      <- Console[IO].println(answer.message)
      } yield ExitCode.Success
    }

}
