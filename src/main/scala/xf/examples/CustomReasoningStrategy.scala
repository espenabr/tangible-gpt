package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import xf.examples.Common.{clientResource, tangibleClient, extractKey}
import xf.interactionhandlers.SimpleTextHandler
import xf.model.ReasoningStrategy

object CustomReasoningStrategy extends IOApp:

  /**
   * Use the "think step by step" strategy
   * 
   * Example is borrowed from the OpenAI Cookbook
   */
  
  override def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val tc = tangibleClient(client, extractKey(args))
      for
        response  <- tc.chat(
          "A juggler has 16 balls. Half of the balls are golf balls and half of the golf balls are blue. How many blue golf balls are there?",
          SimpleTextHandler.simpleTextHandler,
          reasoningStrategy = ReasoningStrategy.ThinkStepByStep
        )
        _ <- IO.println(response.rawMessage)
      yield ExitCode.Success
    }
