package tangiblegpt.legacy.interactionhandlers

import tangiblegpt.legacy.model.InteractionHandler

object SimpleTextHandler:

  val simpleTextHandler = new InteractionHandler[String, String](
    s => s,
    s => "",
    (_, b) => Some(b)
  )
