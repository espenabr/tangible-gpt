package xf.examples

import cats.effect.{IO, Resource}
import xf.InteractionClient
import xf.gpt.GptApiClient
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

object Common {

  implicit val logging: LoggerFactory[IO] = Slf4jFactory.create[IO]

  def createInteractionClient(client: Client[IO], apiKey: String): InteractionClient[IO] =
    new InteractionClient[IO](new GptApiClient[IO](client, apiKey))

  def extractKey(args: List[String]): String =
    if args.nonEmpty && args.head.startsWith("sk-") then args.head
    else
      println("OpenAPI key must be provided as argument!")
      ""

  val clientResource: Resource[IO, Client[IO]] = EmberClientBuilder
    .default[IO]
    .build

}
