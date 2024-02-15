package xf.gpt

import org.http4s.implicits.uri
import org.http4s.{AuthScheme, Credentials, Entity, EntityDecoder, EntityEncoder, Headers, MediaType, Method, Request}
import org.http4s.client.Client
import org.http4s.headers.{`Content-Type`, Authorization}
import org.http4s.circe.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax.*
import cats.effect.Concurrent
import GptApiClient.Model.*
import GptApiClient.Model.Role.{Assistant, Function, System, User}
import xf.gpt.GptApiClient.Model.Property.{EnumParameter, IntParameter, StringParameter}

class GptApiClient[F[_]: Concurrent](client: Client[F], val openAiKey: String) {

  private val entityEncoder: EntityEncoder[F, CompletionRequest] = jsonEncoderOf[CompletionRequest]

  given EntityDecoder[F, CompletionResponse] = jsonOf[F, CompletionResponse]

  def chatCompletions(messages: List[Message], tools: Option[List[Tool]] = None): F[CompletionResponse] =
    val body = CompletionRequest("gpt-4", messages, None) // gpt-4 also works

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
        messages: List[Message],
        tools: Option[List[Tool]]
    )

    object CompletionRequest {
      given Encoder[CompletionRequest] = Encoder { r =>
        Json.obj(
          "model"    := r.model,
          "messages" := r.messages,
          "tools"    := r.tools
        )
      }
    }

    enum Property:
      case StringParameter(description: String)
      case EnumParameter(description: String, _enum: List[String])
      case IntParameter(description: String)

    case class Parameters(
        properties: Map[String, Property],
        required: List[String]
    )
    object Parameters:
      given Encoder[Map[String, Property]] = Encoder.instance { map =>
        Json.obj(map.map { case (key, value) =>
          key -> value.asJson(propertyEncoder)
        }.toSeq: _*)
      }

      given Encoder[Parameters] = Encoder { p =>
        Json.obj(
          "type"       := "object",
          "properties" := p.properties
        )
      }

    val propertyEncoder: Encoder[Property] = Encoder.instance {
      case StringParameter(description)      =>
        Json.obj(
          "type"        := "string",
          "description" := description
        )
      case EnumParameter(description, _enum) =>
        Json.obj(
          "type"        := "string",
          "description" := description,
          "num"         := _enum
        )
      case IntParameter(description)         =>
        Json.obj(
          "type"        := "int",
          "description" := description
        )
    }

    case class ToolFunction(
        name: String,
        description: Option[String],
        parameters: Parameters
    )
    object ToolFunction:
      given Encoder[Property]     = propertyEncoder
      given Encoder[ToolFunction] = Encoder { f =>
        Json.obj(
          "name"        := f.name,
          "description" := f.description,
          "parameters"  := f.parameters
        )
      }

    case class Tool(
        function: ToolFunction
    )
    object Tool:
      given Encoder[Tool] = Encoder { t =>
        Json.obj(
          "type"     := "function",
          "function" := t.function
        )
      }

    /*
  "tools": [
    {
      "type": "function",
      "function": {
        "name": "get_current_weather",
        "description": "Get the current weather in a given location",
        "parameters": {
          "type": "object",
          "properties": {
            "location": {
              "type": "string",
              "description": "The city and state, e.g. San Francisco, CA"
            },
            "unit": {
              "type": "string",
              "enum": ["celsius", "fahrenheit"]
            }
          },
          "required": ["location"]
        }
      }
    }
  ],
     */

    enum FinishReason:
      case Stop, ToolCalls

    object FinishReason:
      def toEnum(s: String): Option[FinishReason] =
        Option(s) collect {
          case "stop" => Stop
          case "tool_calls" => ToolCalls
        }

      given Decoder[FinishReason] = Decoder.decodeString.map(s => FinishReason.toEnum(s).get)

    case class Choice(
        index: Int,
        message: Message,
        finishReason: FinishReason
    )

    object Choice {
      given Decoder[Choice] = Decoder { c =>
        for {
          index        <- c.downField("index").as[Int]
          message      <- c.downField("message").as[Message]
          finishReason <- c.downField("finish_reason").as[FinishReason]
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
