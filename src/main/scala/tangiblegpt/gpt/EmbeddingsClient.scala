package tangiblegpt.gpt

import cats.effect.Concurrent
import org.http4s.headers.{`Content-Type`, Authorization}
import org.http4s.implicits.uri
import org.http4s.circe.*
import org.http4s.{
  AuthScheme,
  Credentials,
  Entity,
  EntityDecoder,
  EntityEncoder,
  Headers,
  HttpVersion,
  MediaType,
  Method,
  Request
}
import org.http4s.client.Client
import tangiblegpt.gpt.GptApiClient.Response.{EmbeddingsRequest, EmbeddingsResponse}

class EmbeddingsClient[F[_]: Concurrent](client: Client[F], val openAiKey: String):

  private val entityEncoder: EntityEncoder[F, EmbeddingsRequest] = jsonEncoderOf[EmbeddingsRequest]

  given EntityDecoder[F, EmbeddingsResponse] = jsonOf[F, EmbeddingsResponse]

  def embeddings(text: String): F[EmbeddingsResponse] =
    val body = EmbeddingsRequest(text, "text-embedding-3-large")

    val request = Request[F](
      method = Method.POST,
      uri = uri"https://api.openai.com/v1/embeddings",
      headers = Headers(
        Authorization(Credentials.Token(AuthScheme.Bearer, openAiKey)),
        `Content-Type`(MediaType.application.json)
      ),
      entity = entityEncoder.toEntity(body)
    )

    client.expect[EmbeddingsResponse](request)
