package tangiblegpt.model

import tangiblegpt.gpt.GptApiClient.Common.Message

class ChatResponse[A](
    val value: Option[A],
    val rawMessage: String,
    val history: List[Message]
)