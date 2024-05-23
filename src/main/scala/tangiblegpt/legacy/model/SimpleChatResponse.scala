package tangiblegpt.legacy.model

import tangiblegpt.gpt.GptApiClient.Common.Message

case class SimpleChatResponse(
    message: Message,
    history: List[Message]
)