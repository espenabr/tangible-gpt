package tangiblegpt.model

import tangiblegpt.gpt.GptApiClient.Common.Message

case class TangibleResponse[T](
    value: T,
    rawMessage: String,
    history: List[Message]
)