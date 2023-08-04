package xf.examples

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import xf.examples.Common.{clientResource, createConversationClient, extractKey}
import xf.Input.{collectAnswers, prompt}
import cats.implicits.*
import xf.interactionhandlers.AnswerQuestions.answerQuestionsHandler
import xf.interactionhandlers.RequestQuestions.QuestionType.YesNoQuestions
import xf.interactionhandlers.RequestQuestions.{
  requestFollowupQuestionsHandler,
  QuestionExpectingFollowupQuestions,
  QuestionType
}

object FollowUpQuestionsWithYesNoAnswers extends IOApp {

  /*
   * Flow:
   *  1. User asks a question
   *  2. GPT response with followup questions with the intention of giving a better answer
   *  3. User answers followup questions
   *  4. GPT gives a conclusive answer
   */

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val interactions = createConversationClient(client, extractKey(args))
      for {
        question         <- prompt("Ask a questions and get follow-up questions")
        questionsFromGpt <- interactions.chat(
                              QuestionExpectingFollowupQuestions(question, YesNoQuestions, None),
                              requestFollowupQuestionsHandler
                            )
        myAnswers        <- collectAnswers(questionsFromGpt.value.get)
        answerFromGpt    <- interactions.chat(
                              myAnswers,
                              answerQuestionsHandler,
                              questionsFromGpt.history
                            )
        _                <- Console[IO].println(s"${answerFromGpt.value.get}")
      } yield ExitCode.Success
    }

}
