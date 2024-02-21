package xf.model

import xf.gpt.GptApiClient.Common.Message

case class MessageExchange(
    message: Message,
    reply: Message
)