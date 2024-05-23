package tangiblegpt.examples

import cats.effect.{ExitCode, IO, IOApp}
import io.circe.generic.semiauto.deriveCodec
import cats.implicits.*
import io.circe.Codec
import Common.{clientResource, createTangibleClient, extractKey}
import tangiblegpt.model.{FailedInteraction, TangibleEitherResponse}

object JsonEitherResponse extends IOApp.Simple:

  override val run: IO[Unit] =
    clientResource.use { client =>
      case class Answer(answer: String)
      given Codec[Answer] = deriveCodec

      case class Clarifications(clarifyingQuestions: List[String])
      given Codec[Clarifications] = deriveCodec

      val tc = createTangibleClient(client, extractKey())

      tc.expectJsonEither(
        "How much does a fish weigh?",
        Answer("answer"),
        Clarifications(List("question1", "question2", "questionN"))
      ).map { (response: Either[FailedInteraction, TangibleEitherResponse[Answer, Clarifications]]) =>
        ???
      }

      for
        response <- tc.expectJsonEither(
          "How much does a fish weigh?",
          Answer("answer"),
          Clarifications(List("question", "question"))
        )
        _ = response.foreach(r => println(r.value))
      yield ()
    }
