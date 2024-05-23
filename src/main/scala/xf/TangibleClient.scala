package xf

import xf.gpt.GptApiClient
import cats.effect.Concurrent
import cats.implicits.*
import io.circe
import io.circe.{Decoder, Encoder}
import io.circe.parser.decode
import io.circe.syntax.*
import xf.gpt.GptApiClient.Common.{Message, Role}
import xf.gpt.GptApiClient.Common.Message.{ContentMessage, ResultFromToolMessage}
import xf.gpt.GptApiClient.Common.Role.User
import xf.gpt.GptApiClient.Request.Property.{EnumProperty, IntegerProperty, StringProperty}
import xf.gpt.GptApiClient.Request.{Parameters, Property, RequestFunction, Tool}
import xf.gpt.GptApiClient.Response.FinishReason
import xf.gpt.GptApiClient.Response.FinishReason.Choice.{StopChoice, ToolCallsChoice}
import xf.gpt.GptApiClient.Response.FinishReason.CompletionResponse
import xf.model.FailedInteraction.ParseError
import xf.model.Param.{EnumParam, IntegerParam, StringParam}
import xf.model.ReasoningStrategy.{Simple, SuggestMultipleAndPickOne, ThinkStepByStep}
import xf.model.{
  ChatResponse,
  FailedInteraction,
  FunctionCall,
  InteractionHandler,
  ReasoningStrategy,
  SimpleChatResponse,
  TangibleEitherResponse,
  TangibleOptionResponse,
  TangibleResponse
}

import scala.util.Try

class TangibleClient[F[_]: Concurrent](gptApiClient: GptApiClient[F]):

  def expectJson[R](
      prompt: String,
      example: R,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  )(implicit
      decoder: Decoder[R],
      encoder: Encoder[R]
  ): F[Either[FailedInteraction, TangibleResponse[R]]] =
    def encodeToJson(value: R): String = value.asJson.noSpaces

    val responseFormatDescription =
      s"""There response must be in valid JSON and only JSON, nothing else
         |
         |Example:
         |${encodeToJson(example)}""".stripMargin
    val message                   = userContentMessage(initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)))

    interact(message, history, functionCalls, reasoningStrategy, Some(responseFormatDescription)).map { response =>
      decode[R](response.message.content)
        .map { decoded =>
          TangibleResponse[R](
            decoded,
            response.message.content,
            response.history
          )
        }
        .leftMap(_ => ParseError(response.message.content, response.history))
    }

  def expectJsonOption[R](
      prompt: String,
      example: R,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  )(implicit
      decoder: Decoder[R],
      encoder: Encoder[R]
  ): F[Either[FailedInteraction, TangibleOptionResponse[R]]] =
    def encodeToJson(value: R): String = value.asJson.noSpaces
    val responseFormatDescription      =
      s"""If you don't, know the answer, simply say "I don't know". Otherwise, the response must be in valid JSON and
         |only JSON, nothing else.
         |
         |Example:
         |${encodeToJson(example)}""".stripMargin
    val message                        = userContentMessage(initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)))

    interact(message, history, functionCalls, reasoningStrategy, Some(responseFormatDescription)).map { response =>
      if iDontKnow(response) then Right(TangibleOptionResponse[R](None, response.message.content, response.history))
      else
        decode[R](response.message.content)
          .map { decoded =>
            TangibleOptionResponse[R](
              Some(decoded),
              response.message.content,
              response.history
            )
          }
          .leftMap(_ => ParseError(response.message.content, response.history))
    }

  private def iDontKnow(response: SimpleChatResponse): Boolean =
    response.message.content.toLowerCase.trim.contains("i don't know")

  def expectJsonEither[L, R](
      prompt: String,
      leftExample: L,
      rightExample: R,
      history: List[Message],
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  )(implicit
      leftDecoder: Decoder[L],
      leftEncoder: Encoder[L],
      rightDecoder: Decoder[R],
      rightEncoder: Encoder[R]
  ): F[Either[FailedInteraction, TangibleEitherResponse[L, R]]] =
    def encodeLeftToJson(value: L): String  = value.asJson.noSpaces
    def encodeRightToJson(value: R): String = value.asJson.noSpaces

    val responseFormatDescription =
      s"""The response must be valid JSON and JSON only.
         |Depending on the answer, it may either have this format (example): ${encodeLeftToJson(leftExample)}
         |Or this format (example): ${encodeRightToJson(rightExample)}""".stripMargin
    val message                   = userContentMessage(initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)))

    interact(message, history, functionCalls, reasoningStrategy, Some(responseFormatDescription)).map { response =>
      val decodedLeft: Either[circe.Error, L]  = decode[L](response.message.content)
      val decodedRight: Either[circe.Error, R] = decode[R](response.message.content)
      def toResponse(value: Either[L, R])      = TangibleEitherResponse(value, response.message.content, response.history)

      (decodedLeft.toOption.map(l => toResponse(Left(l))) orElse decodedRight.toOption.map(r => toResponse(Right(r))))
        .toRight(ParseError(response.message.content, response.history))
    }

  def expectPlainText(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[ChatResponse[String]] =
    val message = userContentMessage(initialPrompt(reasoningStrategy, prompt, None))
    interact(message, history, functionCalls, reasoningStrategy, None).map { response =>
      ChatResponse[String](
        Some(response.message.content),
        response.message.content,
        response.history
      )
    }

  def expectPlainTextOption(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[Either[FailedInteraction, TangibleOptionResponse[String]]] =
    val responseFormatDescription =
      s"""If you don't know the answer, simply say "I don't know"""".stripMargin
    val message                   = userContentMessage(initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)))

    interact(message, history, functionCalls, reasoningStrategy, Some(responseFormatDescription)).map { response =>
      val value = if iDontKnow(response) then None else Some(response.message.content)
      Right(
        TangibleOptionResponse[String](
          value,
          response.message.content,
          response.history
        )
      )
    }

  def expectBoolean(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[Either[FailedInteraction, TangibleResponse[Boolean]]] =
    val responseFormatDescription = """I only want a yes or no answer, nothing else. Reply with either "yes" or "no""""
    val message                   = userContentMessage(initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)))

    interact(message, history, functionCalls, reasoningStrategy, Some(responseFormatDescription)).map { response =>
      parseBoolean(response.message.content) match
        case Some(b) => Right(TangibleResponse[Boolean](b, response.message.content, response.history))
        case None    => Left(ParseError(response.message.content, response.history))
    }

  def expectBooleanOption(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[Either[FailedInteraction, TangibleOptionResponse[Boolean]]] =
    val responseFormatDescription =
      s"""If you don't know the answer, simply reply with "I don't know", nothing else.
         |I only want a yes or no answer, nothing else. Reply with either "yes" or "no"""".stripMargin
    val message                   = userContentMessage(initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)))

    interact(message, history, functionCalls, reasoningStrategy, Some(responseFormatDescription)).map { response =>
      def toResponse(value: Option[Boolean]) =
        TangibleOptionResponse[Boolean](value, response.message.content, response.history)

      if iDontKnow(response) then Right(toResponse(None))
      else
        parseBoolean(response.message.content) match
          case Some(b) => Right(toResponse(Some(b)))
          case None    => Left(ParseError(response.message.content, response.history))
    }

  def expectDouble(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[Either[FailedInteraction, TangibleResponse[Double]]] =
    val responseFormatDescription = "I only want a number as an answer, nothing else."
    val message                   = userContentMessage(initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)))

    interact(message, history, functionCalls, reasoningStrategy, Some(responseFormatDescription)).map { response =>
      Try { response.message.content.toDouble }.toOption
        .map { d =>
          Right(TangibleResponse(d, response.message.content, response.history))
        }
        .getOrElse(Left(ParseError(response.message.content, response.history)))
    }

  def expectDoubleOption(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[Either[FailedInteraction, TangibleOptionResponse[Double]]] =
    val responseFormatDescription =
      s"""If you don't know the answer, simply reply with "I don't know", nothing else.
         |I only want a number as an answer, nothing else.""".stripMargin
    val message                   = userContentMessage(initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)))

    interact(message, history, functionCalls, reasoningStrategy, Some(responseFormatDescription)).map { response =>
      val content                           = response.message.content
      def toResponse(value: Option[Double]) = TangibleOptionResponse(value, content, response.history)

      if iDontKnow(response) then Right(toResponse(None))
      else
        Try { content.toDouble }.toOption
          .map { d =>
            Right(TangibleOptionResponse(Some(d), content, response.history))
          }
          .getOrElse(Left(ParseError(content, response.history)))
    }

  private def parseBoolean(s: String): Option[Boolean] = s.toLowerCase match
    case "false" | "no" | "n" | "0" => Some(false)
    case "true" | "yes" | "y" | "1" => Some(true)
    case _                          => None

  private def interact(
      message: Message,
      history: List[Message],
      functionCalls: List[FunctionCall[F]],
      reasoningStrategy: ReasoningStrategy,
      responseFormatDescription: Option[String]
  ): F[SimpleChatResponse] =
    for
      response <- withPossibleFunctionCalls(message, history, functionCalls)

      finalResponse <- afterPossibleReasoning(response, reasoningStrategy, responseFormatDescription)
    yield finalResponse

  private def withPossibleFunctionCalls(
      message: Message,
      history: List[Message],
      functionCalls: List[FunctionCall[F]]
  ): F[SimpleChatResponse] =
    if functionCalls.isEmpty then plainTextChat(message, history)
    else chatWithFunctionCalls(message, functionCalls, history)

  private def initialPrompt(
      reasoningStrategy: ReasoningStrategy,
      prompt: String,
      responseFormatDescription: Option[String]
  ): String =
    reasoningStrategy match
      case Simple                    =>
        responseFormatDescription match
          case Some(formatDescription) =>
            s"""$prompt
               |
               |$formatDescription""".stripMargin
          case None                    => prompt
      case ThinkStepByStep           =>
        s"""$prompt
           |
           |Let's think step by step""".stripMargin
      case SuggestMultipleAndPickOne =>
        s"""$prompt
           |
           |Give me some alternative answers to this that make sense. Enumerate them.""".stripMargin

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

  private def afterPossibleReasoning(
      response: SimpleChatResponse,
      reasoningStrategy: ReasoningStrategy,
      responseFormatDescription: Option[String]
  ) =
    reasoningStrategy match
      case Simple                    => SimpleChatResponse(response.message, response.history).pure[F]
      case ThinkStepByStep           =>
        plainTextChat(
          userContentMessage(
            responseFormatDescription
              .map(rfd => s"""Give me an answer.
                 |
                 |$rfd""".stripMargin)
              .getOrElse("Give me an answer.")
          ),
          response.history
        )
      case SuggestMultipleAndPickOne =>
        plainTextChat(
          userContentMessage(
            responseFormatDescription
              .map(rfd => s"""Pick the best answer.
                 |
                 |$responseFormatDescription""".stripMargin)
              .getOrElse("Pick the best answer.")
          ),
          response.history
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

  private def lastMessage(response: CompletionResponse) =
    response.choices.last match
      case sc: StopChoice      => sc.message
      case tc: ToolCallsChoice => tc.message
