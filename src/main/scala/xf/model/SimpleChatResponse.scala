package xf.model

import xf.gpt.GptApiClient.Common.Message

case class SimpleChatResponse(
    message: Message,
    history: List[Message]
)