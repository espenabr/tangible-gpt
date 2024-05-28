package tangiblegpt.gpt

import org.http4s.implicits.uri
import org.http4s.{AuthScheme, Credentials, Entity, EntityDecoder, EntityEncoder, Headers, MediaType, Method, Request}
import org.http4s.client.Client
import org.http4s.headers.{`Content-Type`, Authorization}
import org.http4s.circe.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax.*
import cats.effect.Concurrent
import tangiblegpt.gpt.GptApiClient.Common.Message.{ContentMessage, ToolCallsMessage}
import tangiblegpt.gpt.GptApiClient.Common.Message
import tangiblegpt.gpt.GptApiClient.Request.{CompletionRequest, Tool}
import tangiblegpt.gpt.GptApiClient.Request.Property.{EnumProperty, IntegerProperty, StringProperty}
import tangiblegpt.gpt.GptApiClient.Response.{CompletionResponse, ToolCall}

class GptApiClient[F[_]: Concurrent](client: Client[F], val openAiKey: String):

  private val entityEncoder: EntityEncoder[F, CompletionRequest] = jsonEncoderOf[CompletionRequest]

  given EntityDecoder[F, CompletionResponse] = jsonOf[F, CompletionResponse]

  /** Request to GPT Chat completion API
    *
    * @param messages
    *   all messages in conversation
    * @param tools
    *   functions that GPT may call if it finds it feasible
    * @return
    *   response
    */
  def chatCompletions(
      messages: List[Message],
      tools: Option[List[Tool]] = None
  ): F[CompletionResponse] =
    val body = CompletionRequest("gpt-4", messages, tools)

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

object GptApiClient:

  object Common:
    enum Role:
      case System, User, Assistant, Tool, Function

    object Role:
      given Encoder[Role] = Encoder.encodeString.contramap {
        case Role.System    => "system"
        case Role.Assistant => "assistant"
        case Role.User      => "user"
        case Role.Tool      => "tool"
        case Role.Function  => "function"
      }

    enum Message(val role: Role, val content: String):
      case ContentMessage(
          override val role: Role,
          override val content: String
      ) extends Message(role, content)
      case ToolCallsMessage(
          override val role: Role,
          toolCalls: List[ToolCall]
      ) extends Message(role, "")
      case ResultFromToolMessage(
          override val role: Role,
          name: String,
          override val content: String,
          toolCallId: String
      ) extends Message(role, content)

    object Message:
      given Encoder[Message] = Encoder {
        case ContentMessage(role, content)                          =>
          Json.obj(
            "role"    := role,
            "content" := content
          )
        case ToolCallsMessage(role, toolCalls)                      =>
          Json.obj(
            "role"       := role,
            "tool_calls" := toolCalls
          )
        case ResultFromToolMessage(role, name, content, toolCallId) =>
          Json.obj(
            "tool_call_id" := toolCallId,
            "role"         := role,
            "name"         := name,
            "content"      := content
          )
      }

      given Decoder[ContentMessage]   = Decoder { c =>
        for
          role    <- c.downField("role").as[Role]
          content <- c.downField("content").as[String]
        yield ContentMessage(role, content)
      }
      given Decoder[ToolCallsMessage] = Decoder { c =>
        for
          role      <- c.downField("role").as[Role]
          toolCalls <- c.downField("tool_calls").as[List[ToolCall]]
        yield ToolCallsMessage(role, toolCalls)
      }

    def parseRole(s: String): Either[String, Role] =
      s match {
        case "system"    => Right(Role.System)
        case "assistant" => Right(Role.Assistant)
        case "user"      => Right(Role.User)
        case "tool"      => Right(Role.Tool)
        case _           => Left(s"Could not decode $s as Role")
      }

    given Decoder[Role] = Decoder { c =>
      for
        str  <- c.as[String]
        role <- parseRole(str).left.map(DecodingFailure(_, c.history))
      yield role
    }

  object Request:

    case class RequestFunction(name: String, description: Option[String], parameters: Parameters)
    object RequestFunction:
      given Encoder[RequestFunction] = Encoder { f =>
        Json.obj(
          "name"        := f.name,
          "description" := f.description,
          "parameters"  := f.parameters
        )
      }

    val propertyEncoder: Encoder[Property] = Encoder.instance {
      case StringProperty(description)      =>
        Json.obj(
          "type"        := "string",
          "description" := description
        )
      case EnumProperty(description, _enum) =>
        Json.obj(
          "type"        := "string",
          "description" := description,
          "num"         := _enum
        )
      case IntegerProperty(description)     =>
        Json.obj(
          "type"        := "integer",
          "description" := description
        )
    }

    given Encoder[Property] = propertyEncoder

    case class Tool(function: RequestFunction)
    object Tool:
      given Encoder[Tool] = Encoder { t =>
        Json.obj(
          "type"     := "function",
          "function" := t.function
        )
      }

    enum Property:
      case StringProperty(description: String)
      case EnumProperty(description: String, _enum: List[String])
      case IntegerProperty(description: String)

    case class Parameters(properties: Map[String, Property], required: List[String])
    object Parameters:
      given Encoder[Map[String, Property]] = Encoder.instance { map =>
        Json.obj(map.map { case (key, value) =>
          key -> value.asJson(propertyEncoder)
        }.toSeq: _*)
      }
      given Encoder[Parameters]            = Encoder { p =>
        Json.obj(
          "type"       := "object",
          "properties" := p.properties
        )
      }

    enum ResponseFormat:
      case Auto
      case JsonObject

    object ResponseFormat:
      given Encoder[ResponseFormat] = Encoder.encodeString.contramap {
        case Auto       => "auto"
        case JsonObject => "json_object"
      }

    case class CompletionRequest(
        model: String,
        messages: List[Message],
        tools: Option[List[Tool]]
//        responseFormat: ResponseFormat
    )
    object CompletionRequest:
      given Encoder[CompletionRequest] = Encoder { r =>
        Json.obj(
          "model"    := r.model,
          "messages" := r.messages,
          "tools"    := r.tools
//          "response_format" := r.responseFormat
        )
      }

  object Response:
    enum FinishReason:
      case Stop, ToolCalls
    object FinishReason:
      def toEnum(s: String): Option[FinishReason] =
        Option(s) collect {
          case "stop"       => Stop
          case "tool_calls" => ToolCalls
        }
      given Decoder[FinishReason]                 = Decoder.decodeString.map(s => FinishReason.toEnum(s).get)

    case class CompletionResponse(id: String, model: String, choices: List[Choice], usage: Usage)
    object CompletionResponse:
      given Decoder[CompletionResponse] = Decoder { c =>
        for
          id      <- c.downField("id").as[String]
          model   <- c.downField("model").as[String]
          choices <- c.downField("choices").as[List[Choice]]
          usage   <- c.downField("usage").as[Usage]
        yield CompletionResponse(id, model, choices, usage)
      }

    case class ToolCallFunction(name: String, arguments: String)
    object ToolCallFunction:
      given Decoder[ToolCallFunction] = Decoder { c =>
        for
          name      <- c.downField("name").as[String]
          arguments <- c.downField("arguments").as[String]
        yield ToolCallFunction(name, arguments)
      }
      given Encoder[ToolCallFunction] = Encoder { t =>
        Json.obj(
          "name"      := t.name,
          "arguments" := t.arguments
        )
      }

    case class ToolCall(id: String, function: ToolCallFunction)
    object ToolCall:
      given Decoder[ToolCall] = Decoder { c =>
        for
          id       <- c.downField("id").as[String]
          function <- c.downField("function").as[ToolCallFunction]
        yield ToolCall(id, function)
      }
      given Encoder[ToolCall] = Encoder { t =>
        Json.obj(
          "id"       := t.id,
          "function" := t.function,
          "type"     := "function"
        )
      }

    case class Usage(promptTokens: Int, completionTokens: Int, totalTokens: Int)
    object Usage {
      given Decoder[Usage] = Decoder { c =>
        for
          promptTokens     <- c.downField("prompt_tokens").as[Int]
          completionTokens <- c.downField("completion_tokens").as[Int]
          totalTokens      <- c.downField("total_tokens").as[Int]
        yield Usage(promptTokens, completionTokens, totalTokens)
      }
    }

    enum Choice(val message: Message):
      case StopChoice(index: Int, override val message: ContentMessage)        extends Choice(message)
      case ToolCallsChoice(index: Int, override val message: ToolCallsMessage) extends Choice(message)

    object Choice:
      given Decoder[Choice] = Decoder { c =>
        c.get[FinishReason]("finish_reason").flatMap {
          case FinishReason.Stop      =>
            for
              index        <- c.downField("index").as[Int]
              message      <- c.downField("message").as[ContentMessage]
              finishReason <- c.downField("finish_reason").as[FinishReason]
            yield StopChoice(index, message)
          case FinishReason.ToolCalls =>
            for
              index        <- c.downField("index").as[Int]
              message      <- c.downField("message").as[ToolCallsMessage]
              finishReason <- c.downField("finish_reason").as[FinishReason]
            yield ToolCallsChoice(index, message)
        }
      }

    case class EmbeddingsRequest(
        input: String,
        model: String
    )
    object EmbeddingsRequest:
      given Encoder[EmbeddingsRequest] = Encoder { e =>
        Json.obj(
          "input" := e.input,
          "model" := e.model
        )
      }

    case class EmbeddingsData(
        _object: String,
        index: Int,
        embedding: List[Double]
    )
    object EmbeddingsData:
      given Decoder[EmbeddingsData] = Decoder { c =>
        for
          _object   <- c.downField("object").as[String]
          index     <- c.downField("index").as[Int]
          embedding <- c.downField("embedding").as[List[Double]]
        yield EmbeddingsData(_object, index, embedding)
      }

    case class EmbeddingsUsage(
        promptTokens: Int,
        totalTokens: Int
    )
    object EmbeddingsUsage:
      given Decoder[EmbeddingsUsage] = Decoder { c =>
        for
          promptTokens <- c.downField("prompt_tokens").as[Int]
          totalTokens  <- c.downField("total_tokens").as[Int]
        yield EmbeddingsUsage(promptTokens, totalTokens)
      }

    case class EmbeddingsResponse(
        _object: String,
        data: List[EmbeddingsData],
        model: String,
        usage: EmbeddingsUsage
    )
    object EmbeddingsResponse:
      given Decoder[EmbeddingsResponse] = Decoder { c =>
        for
          _object <- c.downField("object").as[String]
          data    <- c.downField("data").as[List[EmbeddingsData]]
          model   <- c.downField("model").as[String]
          usage   <- c.downField("usage").as[EmbeddingsUsage]
        yield EmbeddingsResponse(_object, data, model, usage)
      }
