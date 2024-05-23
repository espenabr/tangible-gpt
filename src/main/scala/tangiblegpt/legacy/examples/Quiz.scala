package tangiblegpt.legacy.examples

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import tangiblegpt.Input.{collectAnswers, prompt}
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}
import tangiblegpt.legacy.interactionhandlers.AnswerQuestions.answerQuestionsHandler
import tangiblegpt.legacy.interactionhandlers.RequestQuestions.Difficulty.Medium
import tangiblegpt.legacy.interactionhandlers.RequestQuestions.QuestionType.{SingleChoiceQuestions, YesNoQuestions}
import tangiblegpt.legacy.interactionhandlers.RequestQuestions.{QuizRequest, requestQuizQuestions}

object Quiz extends IOApp:

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val tc = createTangibleClient(client, extractKey())
      for {
        topic     <- prompt("Quiz topic")
        questions <- tc.chat(QuizRequest(topic, Medium, SingleChoiceQuestions(Some(3)), Some(6)), requestQuizQuestions)
        answers   <- collectAnswers(questions.value.get)
        result    <- tc.chat(answers, answerQuestionsHandler, history = questions.history)
        _         <- Console[IO].println(s"${result.value.get}")
      } yield ExitCode.Success
    }
