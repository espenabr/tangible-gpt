package tangiblegpt.legacy.interactionhandlers

import io.circe.Encoder
import io.circe.syntax.*
import io.circe.*
import tangiblegpt.legacy.model.InteractionHandler
import tangiblegpt.legacy.parsers.Parsers

object RequestJsonArray:

  def createHandler[T: Encoder: Decoder](
      exampleObject: T,
      objective: Option[String] = None
  ): InteractionHandler[String, List[T]] =
    def encodeToJson(value: T): String = value.asJson.noSpaces

    new InteractionHandler[String, List[T]](
      s => s,
      s => s"""Example:
         |[
         |  ${encodeToJson(exampleObject)},
         |  ...
         |]""".stripMargin,
      (_, response) => Parsers.parseJsonArray(response),
      objective
    )
