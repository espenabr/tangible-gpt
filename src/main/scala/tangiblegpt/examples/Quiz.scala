package tangiblegpt.examples

import cats.effect.{IO, IOApp}
import cats.effect.std.Console
import tangiblegpt.Input
import tangiblegpt.Input.prompt
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}
import tangiblegpt.model.QuestionType.SingleChoiceQuestions

object Quiz extends IOApp.Simple:

  override def run: IO[Unit] = clientResource
    .use { client =>
      val tc = createTangibleClient(client, extractKey())
      for
        topic <- prompt("Quiz topic")
        response <- tc.expectPlainText(
                s"I want you to be a quiz master. Quiz topic: $topic",
                withFollowupQuestions =
                  Some(tc.FollowupQuestions(SingleChoiceQuestions(Some(4)), Some(10), Input.collectAnswers))
              )
        _ <- Console[IO].println(response.value)
      yield ()
    }

