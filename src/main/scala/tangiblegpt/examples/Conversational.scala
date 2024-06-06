package tangiblegpt.examples

import cats.data.EitherT
import cats.effect.{IO, IOApp}
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}
import cats.implicits.*
import tangiblegpt.model.FailedInteraction

import scala.util.Random

object Conversational extends IOApp.Simple:

  val run: IO[Unit] =
    clientResource.use { client =>
      val tc = createTangibleClient(client, extractKey())

      case class ThingToDo(activity: String)
      given Codec[ThingToDo] = deriveCodec

      case class ActivityDetails(activity: String, description: String, difficulty1To10: Int)
      given Codec[ActivityDetails] = deriveCodec

      val result: IO[Either[FailedInteraction, ActivityDetails]] =
        (for
          response1 <-
            EitherT(tc.expectJson("I'm bored, give me some suggestions of things to do", List(ThingToDo("activity"))))
          response2 <- EitherT(
                         tc.expectJson(
                           s"Tell me more about ${Random.shuffle(response1.value).head.activity}",
                           ActivityDetails("activity", "description", 5),
                           history = response1.history
                         )
                       )
        yield response2.value).value

      result.map(_ => ())
    }
