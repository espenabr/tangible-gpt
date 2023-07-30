package xf.model

import xf.Interactions.Model.MessageExchange

class ChatResponse[A](
    val value: Option[A],
    val rawMessage: String,
    val history: List[MessageExchange]
)
