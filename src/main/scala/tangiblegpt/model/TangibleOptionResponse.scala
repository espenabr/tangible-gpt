package tangiblegpt.model

import tangiblegpt.gpt.GptApiClient.Common.Message

case class TangibleOptionResponse[T](
    value: Option[T],
    rawMessage: String,
    history: List[Message]
)
