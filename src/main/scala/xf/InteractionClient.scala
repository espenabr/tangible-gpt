package xf

import xf.gpt.GptApiClient
import cats.effect.Concurrent
import cats.implicits.*
import xf.gpt.GptApiClient.Common.{Message, Role}
import xf.gpt.GptApiClient.Common.Message.{ContentMessage, ResultFromToolMessage, ToolCallsMessage}
import xf.gpt.GptApiClient.Common.Role.User
import xf.gpt.GptApiClient.Request.Property.{EnumProperty, IntegerProperty, StringProperty}
import xf.gpt.GptApiClient.Request.{Parameters, Property, RequestFunction, Tool}
import xf.gpt.GptApiClient.Response.FinishReason
import xf.gpt.GptApiClient.Response.FinishReason.Choice.{StopChoice, ToolCallsChoice}
import xf.gpt.GptApiClient.Response.FinishReason.CompletionResponse
import xf.model.Param.{EnumParam, IntegerParam, StringParam}
import xf.model.{ChatResponse, FunctionCall, InteractionHandler, SimpleChatResponse}

class InteractionClient[F[_]: Concurrent](gptApiClient: GptApiClient[F]) {

  def chat[A, B](
      requestValue: A,
      handler: InteractionHandler[A, B],
      functionCalls: List[FunctionCall[F]] = List.empty,
      history: List[Message] = List.empty
  ): F[ChatResponse[B]] =
    val prompt =
      s"""${handler.objective}
         |
         |${handler.render(requestValue)}
         |
         |${handler.responseFormatDescription(requestValue)}""".stripMargin

    if functionCalls.isEmpty then
      plainTextChat(userContentMessage(prompt), history).map { response =>
        ChatResponse(
          handler.parse(requestValue, response.message.content),
          response.message.content,
          history :+ userContentMessage(prompt) :+ response.message
        )
      }
    else
      chatWithFunctionCalls(ContentMessage(User, prompt), functionCalls, history).map { response =>
        val messageContent = response.message match
          case ContentMessage(_, c)              => c
          case ToolCallsMessage(_, _)            => ""
          case ResultFromToolMessage(_, n, c, _) => c

        ChatResponse(handler.parse(requestValue, messageContent), messageContent, response.history)
      }

  private def userContentMessage(s: String) = ContentMessage(User, s)

  private def chatWithFunctionCalls(
      message: Message,
      functionCalls: List[FunctionCall[F]],
      history: List[Message] = List.empty
  ): F[SimpleChatResponse] =
    val messages = history :+ message
    val tools    =
      functionCalls.map(fc =>
        Tool(
          RequestFunction(
            fc.name,
            Some(fc.description),
            Parameters(
              fc.params.map {
                case IntegerParam(name, description)     => (name, IntegerProperty(description))
                case StringParam(name, description)      => (name, StringProperty(description))
                case EnumParam(name, description, _enum) => (name, EnumProperty(description, _enum))
              }.toMap,
              List.empty
            )
          )
        )
      )

    for
      initialResponse <- gptApiClient.chatCompletions(messages, Some(tools))
      response        <- initialResponse.choices.last match
                           case cm: StopChoice                  => initialResponse.pure[F]
                           case ToolCallsChoice(index, message) =>
                             val returnMessages: F[List[Message]] =
                               (for
                                 toolCall     <- message.toolCalls
                                 functionCall <- functionCalls.filter(_.name === toolCall.function.name)
                               yield
                                 val result: F[String] = functionCall.function(toolCall.function.arguments)
                                 result.map(r => ResultFromToolMessage(Role.Tool, toolCall.function.name, r, toolCall.id))
                               ).sequence

                             returnMessages.flatMap(rm =>
                               gptApiClient.chatCompletions((messages :+ message) ++ rm, Some(tools))
                             )
    yield
      val reply: Message = response.choices.last match
        case StopChoice(_, message)      => message
        case ToolCallsChoice(_, message) => message

      SimpleChatResponse(reply, history :+ message :+ reply)

  def plainTextChat(message: Message, history: List[Message] = List.empty): F[SimpleChatResponse] =
    val messages = history :+ message

    gptApiClient.chatCompletions(messages).map { response =>
      val reply = response.choices.last match
        case sc: StopChoice      => sc.message
        case tc: ToolCallsChoice => tc.message

      SimpleChatResponse(reply, history :+ message :+ reply)
    }

  private def lastMessage(response: CompletionResponse) =
    response.choices.last match
      case sc: StopChoice      => sc.message
      case tc: ToolCallsChoice => tc.message

}
