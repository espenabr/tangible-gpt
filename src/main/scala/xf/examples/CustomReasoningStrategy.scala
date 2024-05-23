package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import xf.examples.Common.{clientResource, createTangibleClient, extractKey}
import xf.model.{FailedInteraction, ReasoningStrategy, TangibleResponse}

object CustomReasoningStrategy extends IOApp:

  /**
   * Use the "think step by step" strategy
   * 
   * Example is borrowed from the OpenAI Cookbook
   */
  
  override def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val tc = createTangibleClient(client, extractKey(args))

      for
        response  <- tc.expectDouble(
          "A juggler has 16 balls. Half of the balls are golf balls and half of the golf balls are blue. How many blue golf balls are there?",
          reasoningStrategy = ReasoningStrategy.ThinkStepByStep
        )
        _ <- IO.println(response.toOption.map(_.value).getOrElse(""))
      yield ExitCode.Success
    }
