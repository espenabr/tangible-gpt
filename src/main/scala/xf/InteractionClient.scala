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
import xf.model.ReasoningStrategy.{None, SuggestMultipleAndPickOne, ThinkStepByStep}
import xf.model.{ChatResponse, FunctionCall, InteractionHandler, ReasoningStrategy, SimpleChatResponse}

class InteractionClient[F[_]: Concurrent](gptApiClient: GptApiClient[F]):

  def chat[A, B](
      requestValue: A,
      handler: InteractionHandler[A, B],
      functionCalls: List[FunctionCall[F]] = List.empty,
      history: List[Message] = List.empty,
      reasoningStrategy: ReasoningStrategy = None
  ): F[ChatResponse[B]] =
    val startOfPrompt =
      s"""${handler.objective}
         |
         |${handler.render(requestValue)}""".stripMargin

    val prompt = reasoningStrategy match
      case None                      =>
        s"""$startOfPrompt
           |
           |${handler.responseFormatDescription(requestValue)}""".stripMargin
      case ThinkStepByStep           =>
        s"""$startOfPrompt
           |
           |Let's think step by step""".stripMargin
      case SuggestMultipleAndPickOne =>
        s"""$startOfPrompt
           |
           |Give me some alternative answers to this that could make sense. Enumerate them.""".stripMargin

    val message = userContentMessage(prompt)

    for
      response <- if functionCalls.isEmpty then plainTextChat(message, history)
                  else chatWithFunctionCalls(message, functionCalls, history)

      finalResponse <- reasoningStrategy match
                         case ThinkStepByStep           =>
                           plainTextChat(
                             userContentMessage(
                               s"""Give me an answer.
                                    |${handler.responseFormatDescription(requestValue)}""".stripMargin
                             ),
                             response.history
                           )
                         case SuggestMultipleAndPickOne =>
                           plainTextChat(
                             userContentMessage(
                               s"""Pick the best answer.
                                    |${handler.responseFormatDescription(requestValue)}""".stripMargin
                             ),
                             response.history
                           )
                         case None                      =>
                           SimpleChatResponse(response.message, response.history).pure[F]
    yield ChatResponse[B](
      handler.parse(requestValue, finalResponse.message.content),
      finalResponse.message.content,
      reasoningStrategy match
        case None => response.history
        case _    => finalResponse.history
    )

  private def userContentMessage(s: String) = ContentMessage(User, s)

  private def functionCallTools(functionCalls: List[FunctionCall[F]]) = functionCalls.map(fc =>
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

  private def callFunctionIfApplicable(
      initialResponse: CompletionResponse,
      functionCalls: List[FunctionCall[F]],
      messages: List[Message]
  ): F[CompletionResponse] =
    initialResponse.choices.last match
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
          gptApiClient.chatCompletions(messages ++ rm, Some(functionCallTools(functionCalls)))
        )

  private def chatWithFunctionCalls(
      message: Message,
      functionCalls: List[FunctionCall[F]],
      history: List[Message] = List.empty
  ): F[SimpleChatResponse] =
    val messages = history :+ message
    val tools    = functionCallTools(functionCalls)
    for
      initialResponse <- gptApiClient.chatCompletions(messages, Some(tools))
      response        <- callFunctionIfApplicable(initialResponse, functionCalls, messages)
    yield
      val reply: Message = response.choices.last.message
      SimpleChatResponse(reply, history :+ message :+ reply)

  def plainTextChat(
      message: Message,
      history: List[Message] = List.empty
  ): F[SimpleChatResponse] =
    val messages = history :+ message

    gptApiClient.chatCompletions(messages).map { response =>
      val reply = response.choices.last.message
      SimpleChatResponse(reply, history :+ message :+ reply)
    }

  private def lastMessage(response: CompletionResponse) =
    response.choices.last match
      case sc: StopChoice      => sc.message
      case tc: ToolCallsChoice => tc.message
