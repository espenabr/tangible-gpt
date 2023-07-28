package xf.examples

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.client.Client
import xf.examples.Common.{clientResource, createConversationClient, extractKey}
import xf.Input.{collectAnswers, prompt}
import cats.implicits.*
import xf.Interactions.Model.ExpectedQuestion.ExpectedBooleanQuestion

object FollowUpQuestionsWithYesNoAnswers extends IOApp {

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val interactions = createConversationClient(client, extractKey(args))
      for {
        question          <- prompt("Ask a questions and get follow-up questions")
        followupQuestions <- interactions.requestQuestionsFromGpt(
                               s"""I have a question, to which I want you to ask me follow-up questions that can help
                                  |you to give me a better answer. My question: $question""".stripMargin,
                               ExpectedBooleanQuestion
                             )
        answers           <- collectAnswers(followupQuestions.questions)
        conclusion        <- interactions.submitAnswersToQuestionsFromGpt("", answers, followupQuestions.history)
        _                 <- Console[IO].println(conclusion.rawMessage)
      } yield ExitCode.Success
    }

}
