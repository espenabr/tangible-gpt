package tangiblegpt

import tangiblegpt.gpt.GptApiClient
import cats.effect.Concurrent
import cats.implicits.*
import io.circe
import io.circe.{Decoder, Encoder}
import io.circe.parser.decode
import io.circe.syntax.*
import tangiblegpt.gpt.GptApiClient.Common.{Message, Role}
import tangiblegpt.gpt.GptApiClient.Common.Message.{ContentMessage, ResultFromToolMessage}
import tangiblegpt.gpt.GptApiClient.Common.Role.User
import tangiblegpt.gpt.GptApiClient.Request.Property.{EnumProperty, IntegerProperty, StringProperty}
import tangiblegpt.gpt.GptApiClient.Request.{Parameters, Property, RequestFunction, Tool}
import tangiblegpt.gpt.GptApiClient.Response.FinishReason
import tangiblegpt.gpt.GptApiClient.Response.FinishReason.Choice.{StopChoice, ToolCallsChoice}
import tangiblegpt.gpt.GptApiClient.Response.FinishReason.CompletionResponse
import tangiblegpt.legacy.model.{ChatResponse, InteractionHandler, SimpleChatResponse}
import tangiblegpt.model.FailedInteraction.ParseError
import tangiblegpt.model.Param.{EnumParam, IntegerParam, StringParam}
import tangiblegpt.model.ReasoningStrategy.{Simple, SuggestMultipleAndPickOne, ThinkStepByStep}
import tangiblegpt.model.{
  FailedInteraction,
  FunctionCall,
  ReasoningStrategy,
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

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription)
    ).map { r =>
      decode[R](r.value)
        .map { decoded => TangibleResponse[R](decoded, r.value, r.history) }
        .leftMap(_ => ParseError(r.value, r.history))
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

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription)
    ).map { r =>
      if iDontKnow(r) then Right(TangibleOptionResponse[R](None, r.value, r.history))
      else
        decode[R](r.value)
          .map { decoded => TangibleOptionResponse[R](Some(decoded), r.value, r.history) }
          .leftMap(_ => ParseError(r.value, r.history))
    }

  private def iDontKnow(response: TangibleResponse[String]): Boolean =
    response.value.toLowerCase.trim.contains("i don't know")

  def expectJsonEither[L, R](
      prompt: String,
      leftExample: L,
      rightExample: R,
      history: List[Message] = List.empty,
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

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription)
    ).map { r =>
      val decodedLeft: Either[circe.Error, L]  = decode[L](r.value)
      val decodedRight: Either[circe.Error, R] = decode[R](r.value)
      def toResponse(value: Either[L, R])      = TangibleEitherResponse(value, r.value, r.history)

      (decodedLeft.toOption.map(l => toResponse(Left(l))) orElse decodedRight.toOption.map(r => toResponse(Right(r))))
        .toRight(ParseError(r.value, r.history))
    }

  def expectPlainText(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[TangibleResponse[String]] =
    interact(
      initialPrompt(reasoningStrategy, prompt, None),
      history,
      functionCalls,
      reasoningStrategy,
      None
    ).map { r => TangibleResponse[String](r.value, r.value, r.history) }

  def expectPlainTextOption(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[Either[FailedInteraction, TangibleOptionResponse[String]]] =
    val responseFormatDescription =
      s"""If you don't know the answer, simply say "I don't know"""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription)
    ).map { r =>
      val value = if iDontKnow(r) then None else Some(r.value)
      Right(TangibleOptionResponse[String](value, r.value, r.history))
    }

  def expectBoolean(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[Either[FailedInteraction, TangibleResponse[Boolean]]] =
    val responseFormatDescription =
      """I only want a yes or no answer, nothing else. Reply with either "yes" or "no""""

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription)
    ).map { r =>
      parseBoolean(r.value) match
        case Some(b) => Right(TangibleResponse[Boolean](b, r.value, r.history))
        case None    => Left(ParseError(r.value, r.history))
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

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription)
    ).map { r =>
      def toResponse(value: Option[Boolean]) =
        TangibleOptionResponse[Boolean](value, r.value, r.history)

      if iDontKnow(r) then Right(toResponse(None))
      else
        parseBoolean(r.value) match
          case Some(b) => Right(toResponse(Some(b)))
          case None    => Left(ParseError(r.value, r.history))
    }

  def expectDouble(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[Either[FailedInteraction, TangibleResponse[Double]]] =
    val responseFormatDescription = "I only want a number (all digits) as an answer, nothing else."

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription)
    ).map { r =>
      Try { r.value.toDouble }.toOption
        .map { d => Right(TangibleResponse(d, r.value, r.history)) }
        .getOrElse(Left(ParseError(r.value, r.history)))
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

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription)
    ).map { r =>
      val content                           = r.value
      def toResponse(value: Option[Double]) = TangibleOptionResponse(value, content, r.history)

      if iDontKnow(r) then Right(toResponse(None))
      else
        Try { content.toDouble }.toOption
          .map { d => Right(TangibleOptionResponse(Some(d), content, r.history)) }
          .getOrElse(Left(ParseError(content, r.history)))
    }

  def expectEnumCase[T](
      prompt: String,
      options: List[T],
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[Either[FailedInteraction, TangibleResponse[T]]] =
    val responseFormatDescription =
      s"""I want you to respond with one of the following values, nothing else:
         |${options.map(_.toString).mkString(", ")}
         |""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription)
    ).map { r =>
      val content              = r.value
      def toResponse(value: T) = TangibleResponse(value, content, r.history)

      options
        .find(_.toString.toLowerCase() === content.trim.toLowerCase())
        .map(e => Right(toResponse(e)))
        .getOrElse(Left(ParseError(content, history)))
    }

  def expectEnumCaseOption[T](
      prompt: String,
      options: List[T],
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[Either[FailedInteraction, TangibleOptionResponse[T]]] =
    val responseFormatDescription =
      s"""If you don't know, simply reply "I don't know", nothing else.
         |Otherwise, I want you to respond with one of the following values, nothing else:
         |${options.map(_.toString).mkString(", ")}
         |""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription)
    ).map { r =>
      val content = r.value

      def toResponse(value: Option[T]) = TangibleOptionResponse(value, content, r.history)

      if iDontKnow(r) then Right(toResponse(None))
      else
        options
          .find(_.toString.toLowerCase() === content.trim.toLowerCase())
          .map(e => Right(toResponse(Some(e))))
          .getOrElse(Left(ParseError(content, history)))
    }

  def expectEnumCases[T](
      prompt: String,
      options: List[T],
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[Either[FailedInteraction, TangibleResponse[Set[T]]]] =
    val responseFormatDescription =
      s"""Given the following options:
         |${options.map(_.toString).mkString(", ")}
         |I want you to respond with those that apply. If none of them apply, just say "None".
         |I want a list of options on a single line, separated by comma, and nothing else in the response.""".stripMargin

    interact(
      initialPrompt(reasoningStrategy, prompt, Some(responseFormatDescription)),
      history,
      functionCalls,
      reasoningStrategy,
      Some(responseFormatDescription)
    ).map { r =>
      val content = r.value

      def toResponse(value: Set[T]) = TangibleResponse(value, content, r.history)
      val splitted: Set[String]     = content.split(",").toList.map(_.toLowerCase.strip).toSet
      val allSelectionsValid        = splitted.forall { o => options.exists(_.toString === o) }
      val result                    = options.filter { o => splitted.exists(_ === o.toString.toLowerCase) }.toSet

      if allSelectionsValid then Right(toResponse(result))
      else Left(FailedInteraction.ParseError(content, history))
    }

  private def plainTextChat(
      prompt: String,
      history: List[Message] = List.empty,
      functionCalls: List[FunctionCall[F]] = List.empty,
      reasoningStrategy: ReasoningStrategy = Simple
  ): F[TangibleResponse[String]] =
    val message  = userContentMessage(prompt)
    val messages = history :+ message

    gptApiClient.chatCompletions(messages).map { response =>
      val reply = response.choices.last.message
      TangibleResponse[String](
        reply.content,
        reply.content,
        history :+ message :+ reply
      )
    }

  private def parseBoolean(s: String): Option[Boolean] = s.toLowerCase match
    case "false" | "no" | "n" | "0" => Some(false)
    case "true" | "yes" | "y" | "1" => Some(true)
    case _                          => None

  private def interact(
      prompt: String,
      history: List[Message],
      functionCalls: List[FunctionCall[F]],
      reasoningStrategy: ReasoningStrategy,
      responseFormatDescription: Option[String]
  ): F[TangibleResponse[String]] =
    for
      response      <- withPossibleFunctionCalls(prompt, history, functionCalls)
      finalResponse <- afterPossibleReasoning(response, reasoningStrategy, responseFormatDescription)
    yield finalResponse

  private def withPossibleFunctionCalls(
      prompt: String,
      history: List[Message],
      functionCalls: List[FunctionCall[F]]
  ): F[TangibleResponse[String]] =
    if functionCalls.isEmpty then plainTextChat(prompt, history = history)
    else chatWithFunctionCalls(prompt, functionCalls, history)

  private def chatWithFunctionCalls(
      prompt: String,
      functionCalls: List[FunctionCall[F]],
      history: List[Message] = List.empty
  ): F[TangibleResponse[String]] =

    val message  = userContentMessage(prompt)
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
      val reply: Message         = response.choices.last.message
      val history: List[Message] = message +: response.choices.map(_.message)
      TangibleResponse[String](reply.content, reply.content, history)

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

  private def afterPossibleReasoning(
      response: TangibleResponse[String],
      reasoningStrategy: ReasoningStrategy,
      responseFormatDescription: Option[String]
  ): F[TangibleResponse[String]] =
    reasoningStrategy match
      case Simple                    => response.pure[F]
      case ThinkStepByStep           =>
        expectPlainText(
          responseFormatDescription
            .map(rfd => s"""Give me an answer.
                             |
                             |$rfd""".stripMargin)
            .getOrElse("Give me an answer."),
          response.history
        )
      case SuggestMultipleAndPickOne =>
        expectPlainText(
          responseFormatDescription
            .map(rfd => s"""Pick the best answer.
                             |
                             |$responseFormatDescription""".stripMargin)
            .getOrElse("Pick the best answer."),
          response.history
        )

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
