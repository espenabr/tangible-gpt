package tangiblegpt.examples

import cats.effect.{IO, Resource}
import tangiblegpt.TangibleClient
import tangiblegpt.gpt.GptApiClient
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory
import tangiblegpt.gpt.GptApiClient.Common.Message.ContentMessage
import tangiblegpt.gpt.GptApiClient.Common.Role.User
import tangiblegpt.legacy.InteractionClient

object Common:

  implicit val logging: LoggerFactory[IO] = Slf4jFactory.create[IO]

  def createTangibleClient(
      client: Client[IO],
      apiKey: String
  ): TangibleClient[IO] =
    new TangibleClient[IO](new GptApiClient[IO](client, apiKey))

  def createLegacyInteractionClient(
      client: Client[IO],
      apiKey: String
  ): InteractionClient[IO] =
    new InteractionClient[IO](new GptApiClient[IO](client, apiKey))

  def extractKey(): String =
    sys.env.get("TANGIBLE_GPT_OPENAI_KEY") match
      case Some(key) => key
      case None      =>
        println("Missing environment variable TANGIBLE_GPT_OPENAI_KEY - please set")
        ""

  def msg(content: String) =
    ContentMessage(User, content)

  val clientResource: Resource[IO, Client[IO]] = EmberClientBuilder
    .default[IO]
    .build
