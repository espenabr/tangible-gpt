package xf.model

import xf.gpt.GptApiClient.Common.Message

case class SimpleChatResponse(
    message: String,
    history: List[MessageExchange]
)

case class NewSimpleChatResponse(
    message: Message,
    history: List[NewMessageExchange]
)