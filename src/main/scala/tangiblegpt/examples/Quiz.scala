package tangiblegpt.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}
import tangiblegpt.Input.{collectAnswers, prompt}
import tangiblegpt.interactionhandlers.RequestQuestions.{requestQuizQuestions, QuizRequest}
import tangiblegpt.interactionhandlers.RequestQuestions.QuestionType.{SingleChoiceQuestions, YesNoQuestions}
import tangiblegpt.interactionhandlers.AnswerQuestions.answerQuestionsHandler
import tangiblegpt.interactionhandlers.RequestQuestions.Difficulty.Medium

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
