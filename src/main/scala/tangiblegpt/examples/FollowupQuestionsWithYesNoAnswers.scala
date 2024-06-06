package tangiblegpt.examples

import cats.effect.{IO, IOApp}
import cats.effect.std.Console
import tangiblegpt.Input.{collectAnswers, prompt}
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}
import tangiblegpt.model.QuestionType.YesNoQuestions

object FollowupQuestionsWithYesNoAnswers extends IOApp.Simple:

  override def run: IO[Unit] = clientResource
    .use { client =>
      val tc                = createTangibleClient(client, extractKey())
      val followupQuestions = tc.FollowupQuestions(YesNoQuestions, Some(6), collectAnswers)
      for
        question <- prompt("Ask a questions and get follow-up questions")
        response <- tc.expectItems(question, withFollowupQuestions = Some(followupQuestions))
        _        <- response.map { r => Console[IO].println(r.value) }.getOrElse(IO.raiseError(new RuntimeException()))
      yield ()
    }
