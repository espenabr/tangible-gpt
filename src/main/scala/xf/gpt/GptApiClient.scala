package xf.gpt

import org.http4s.implicits.uri
import org.http4s.{AuthScheme, Credentials, Entity, EntityDecoder, EntityEncoder, Headers, MediaType, Method, Request}
import org.http4s.client.Client
import org.http4s.headers.{`Content-Type`, Authorization}
import org.http4s.circe.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import org.http4s.EntityEncoder
import io.circe.syntax.*
import cats.Monad
import cats.effect.Concurrent
import GptApiClient.Model.*
import GptApiClient.Model.Role.{Assistant, Function, System, User}

class GptApiClient[F[_]: Concurrent](client: Client[F], val openAiKey: String) {

  private val entityEncoder: EntityEncoder[F, CompletionRequest] = jsonEncoderOf[CompletionRequest]

  given EntityDecoder[F, CompletionResponse] = jsonOf[F, CompletionResponse]

  def chatCompletions(messages: List[Message]): F[CompletionResponse] = {
    val body = CompletionRequest("gpt-3.5-turbo", messages) // gpt-4 also works

    val request = Request[F](
      method = Method.POST,
      uri = uri"https://api.openai.com/v1/chat/completions",
      headers = Headers(
        Authorization(Credentials.Token(AuthScheme.Bearer, openAiKey)),
        `Content-Type`(MediaType.application.json)
      ),
      entity = entityEncoder.toEntity(body)
    )

    client.expect[CompletionResponse](request)
  }
}
object GptApiClient {
  object Model {
    enum Role:
      case System, User, Assistant, Function

    case class Message(
        role: Role,
        content: String
    )

    object Message {
      given Encoder[Role] = Encoder.encodeString.contramap {
        case System    => "system"
        case Assistant => "assistant"
        case User      => "user"
        case Function  => "function"
      }

      private def parseRole(s: String): Either[String, Role] =
        s match {
          case "system"    => Right(System)
          case "assistant" => Right(Assistant)
          case "user"      => Right(User)
          case "function"  => Right(Function)
          case _           => Left(s"Could not decode $s as Role")
        }

      given Decoder[Role] = Decoder { c =>
        for {
          str  <- c.as[String]
          role <- parseRole(str).left.map(DecodingFailure(_, c.history))
        } yield role
      }

      given Encoder[Message] = Encoder { m =>
        Json.obj(
          "role"    := m.role,
          "content" := m.content
        )
      }

      given Decoder[Message] = Decoder { c =>
        for {
          role    <- c.downField("role").as[Role]
          content <- c.downField("content").as[String]
        } yield Message(role, content)
      }
    }

    case class CompletionRequest(
        model: String,
        messages: List[Message]
    )

    object CompletionRequest {
      given Encoder[CompletionRequest] = Encoder { r =>
        Json.obj(
          "model"    := r.model,
          "messages" := r.messages
        )
      }
    }

    case class Choice(
        index: Int,
        message: Message,
        finishReason: String
    )

    object Choice {
      given Decoder[Choice] = Decoder { c =>
        for {
          index        <- c.downField("index").as[Int]
          message      <- c.downField("message").as[Message]
          finishReason <- c.downField("finish_reason").as[String]
        } yield Choice(index, message, finishReason)
      }
    }

    case class Usage(
        promptTokens: Int,
        completionTokens: Int,
        totalTokens: Int
    )

    object Usage {
      given Decoder[Usage] = Decoder { c =>
        for {
          promptTokens     <- c.downField("prompt_tokens").as[Int]
          completionTokens <- c.downField("completion_tokens").as[Int]
          totalTokens      <- c.downField("total_tokens").as[Int]
        } yield Usage(promptTokens, completionTokens, totalTokens)
      }
    }

    case class CompletionResponse(
        id: String,
        model: String,
        choices: List[Choice],
        usage: Usage
    )

    object CompletionResponse {
      given Decoder[CompletionResponse] = Decoder { c =>
        for {
          id      <- c.downField("id").as[String]
          model   <- c.downField("model").as[String]
          choices <- c.downField("choices").as[List[Choice]]
          usage   <- c.downField("usage").as[Usage]
        } yield CompletionResponse(id, model, choices, usage)
      }
    }

  }
}
