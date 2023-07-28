package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import xf.Interactions
import xf.Interactions.Model.ExpectedQuestion.ExpectedSingleChoiceQuestion
import xf.Interactions.Model.ExpectedFormat.NumberFormat
import xf.gpt.GptApiClient
import xf.examples.Common.{clientResource, createConversationClient, extractKey}
import xf.Input.{collectAnswers, prompt, readFileContent}
import xf.Interactions.Model.Data.NumberData

object Quiz extends IOApp {

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val interactions = createConversationClient(client, extractKey(args))
      for {
        subject  <- prompt("Quiz subject")
        response <- interactions.requestQuestionsFromGpt(
                      description(subject),
                      ExpectedSingleChoiceQuestion(Some(3)),
                      Some(6)
                    )
        answers  <- collectAnswers(response.questions)
        result   <- interactions.submitAnswersToQuestionsFromGpt(
                      "What was my score in percentage?",
                      answers,
                      response.history,
                      NumberFormat
                    )
        _        <- Console[IO].println((result.data match {
                      case NumberData(n) => Some(n)
                      case _             => None
                    }).getOrElse(""))
      } yield ExitCode.Success
    }

  private def description(subject: String) =
    s"""You are a quiz master and will ask me questions.
       |Subject: $subject
       |Difficulty: Medium
       |
       |Score me and congratulate me if I do well.""".stripMargin

}
