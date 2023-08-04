package xf.model

class ChatResponse[A](
    val value: Option[A],
    val rawMessage: String,
    val history: List[MessageExchange]
)
