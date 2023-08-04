package xf.model

case class SimpleChatResponse(
    message: String,
    history: List[MessageExchange]
)
