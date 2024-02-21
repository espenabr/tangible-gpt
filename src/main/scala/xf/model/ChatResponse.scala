package xf.model

import xf.gpt.GptApiClient.Common.Message

class ChatResponse[A](
    val value: Option[A],
    val rawMessage: String,
    val history: List[MessageExchange]
)

class NewChatResponse[A](
    val value: Option[A],
    val rawMessage: String,
    val history: List[Message]
)
