package tangiblegpt.legacy

import cats.effect.Concurrent
import tangiblegpt.gpt.GptApiClient
import tangiblegpt.gpt.GptApiClient.Common.{Message, Role}
import tangiblegpt.model.{FunctionCall, ReasoningStrategy}
import ReasoningStrategy.{Simple, SuggestMultipleAndPickOne, ThinkStepByStep}
import tangiblegpt.gpt.GptApiClient.Common.Role.User
import tangiblegpt.model.Param.{EnumParam, IntegerParam, StringParam}
import tangiblegpt.gpt.GptApiClient.Request.Property.{EnumProperty, IntegerProperty, StringProperty}
import cats.implicits.*
import tangiblegpt.gpt.GptApiClient.Response.FinishReason.CompletionResponse
import tangiblegpt.gpt.GptApiClient.Response.FinishReason.Choice.{StopChoice, ToolCallsChoice}
import tangiblegpt.gpt.GptApiClient.Common.Message.{ContentMessage, ResultFromToolMessage}
import tangiblegpt.gpt.GptApiClient.Request.{Parameters, RequestFunction, Tool}
import tangiblegpt.legacy.model.{ChatResponse, InteractionHandler, SimpleChatResponse}


class InteractionClient[F[_]: Concurrent](gptApiClient: GptApiClient[F]):

  /** Interact with GPT
    *
    * @param requestValue
    *   value for request
    * @param handler
    *   interaction handler to use
    * @param functionCalls
    *   available functions that GPT can call if feasible
    * @param history
    *   history of all previous messages
    * @param reasoningStrategy
    *   custom reasoning strategy to improve the solve rate. ReasoningStrategy.None means to custom strategy
    * @tparam A
    *   request type
    * @tparam B
    *   response type
    * @return
    *   response
    */
  def chat[A, B](
      requestValue: A,
      handler: InteractionHandler[A, B],
      functionCalls: List[FunctionCall[F]] = List.empty,
      history: List[Message] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[ChatResponse[B]] =
    val startOfPrompt =
      s"""${handler.objective}
         |
         |${handler.render(requestValue)}""".stripMargin

    val prompt = reasoningStrategy match
      case Simple                    =>
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
                         case Simple                    =>
                           SimpleChatResponse(response.message, response.history).pure[F]
    yield ChatResponse[B](
      handler.parse(requestValue, finalResponse.message.content),
      finalResponse.message.content,
      reasoningStrategy match
        case Simple => response.history
        case _      => finalResponse.history
    )

  private def userContentMessage(s: String) = ContentMessage(User, s)

  /** Interact with GPT using plain text
    *
    * @param message
    *   message to send
    * @param history
    *   all previous messages in conversation
    * @return
    *   response from GPT
    */
  def plainTextChat(
      message: Message,
      history: List[Message] = List.empty
  ): F[SimpleChatResponse] =
    val messages = history :+ message

    gptApiClient.chatCompletions(messages).map { response =>
      val reply = response.choices.last.message
      SimpleChatResponse(reply, history :+ message :+ reply)
    }

  private def chatWithFunctionCalls(
      message: Message,
      functionCalls: List[FunctionCall[F]],
      history: List[Message] = List.empty
  ): F[SimpleChatResponse] =
    val messages = history :+ message
    val tools    = functionCallTools(functionCalls)
    for
      initialResponse <- gptApiClient.chatCompletions(messages, Some(tools))
      response        <-
        callFunctionIfApplicable(
          initialResponse,
          functionCalls,
          message +: initialResponse.choices.map(_.message)
        )
    yield
      val reply: Message = response.choices.last.message
      SimpleChatResponse(reply, message +: response.choices.map(_.message))

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
      history: List[Message]
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
          gptApiClient.chatCompletions(history ++ rm, Some(functionCallTools(functionCalls)))
        )
