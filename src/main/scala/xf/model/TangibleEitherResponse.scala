package xf.model

import xf.gpt.GptApiClient.Common.Message

case class TangibleEitherResponse[L, R](
    value: Either[L, R],
    rawMessage: String,
    history: List[Message]
)
