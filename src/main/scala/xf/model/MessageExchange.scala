package xf.model

import xf.gpt.GptApiClient.Common.Message

case class MessageExchange(
    message: String,
    reply: String
)

// TODO replace the above one?
case class NewMessageExchange(
    message: Message,
    reply: Message
)