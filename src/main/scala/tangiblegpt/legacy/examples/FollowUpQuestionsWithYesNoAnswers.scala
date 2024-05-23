package tangiblegpt.legacy.examples

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.*
import tangiblegpt.Input.{collectAnswers, prompt}
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}
import tangiblegpt.legacy.interactionhandlers.AnswerQuestions.answerQuestionsHandler
import tangiblegpt.legacy.interactionhandlers.RequestQuestions.QuestionType.YesNoQuestions
import tangiblegpt.legacy.interactionhandlers.RequestQuestions.{QuestionExpectingFollowupQuestions, QuestionType, requestFollowupQuestionsHandler}

object FollowUpQuestionsWithYesNoAnswers extends IOApp:

  /*
   * Flow:
   *  1. User asks a question
   *  2. GPT response with followup questions with the intention of giving a better answer
   *  3. User answers followup questions
   *  4. GPT gives a conclusive answer
   */

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val tc = createTangibleClient(client, extractKey())
      for
        question         <- prompt("Ask a questions and get follow-up questions")
        questionsFromGpt <- tc.chat(
                              QuestionExpectingFollowupQuestions(question, YesNoQuestions, None),
                              requestFollowupQuestionsHandler
                            )
        myAnswers        <- collectAnswers(questionsFromGpt.value.get)
        answerFromGpt    <- tc.chat(
                              myAnswers,
                              answerQuestionsHandler,
                              history = questionsFromGpt.history
                            )
        _                <- Console[IO].println(s"${answerFromGpt.value.get}")
      yield ExitCode.Success
    }
