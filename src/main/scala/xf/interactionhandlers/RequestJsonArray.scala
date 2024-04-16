package xf.interactionhandlers

import io.circe.Encoder
import xf.model.InteractionHandler
import io.circe.syntax.*
import io.circe.*
import xf.parsers.Parsers

object RequestJsonArray:

  def createHandler[T: Encoder: Decoder](prompt: String, exampleObject: T): InteractionHandler[String, List[T]] =
    def encodeToJson(value: T): String = value.asJson.noSpaces

    new InteractionHandler[String, List[T]](
      prompt,
      s => s,
      s => s"""Example:
         |[
         |  ${encodeToJson(exampleObject)},
         |  ...
         |]""".stripMargin,
      (_, response) => Parsers.parseJsonArray(response)
    )
