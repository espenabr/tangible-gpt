package tangiblegpt.model

import tangiblegpt.gpt.GptApiClient.Common.Message

case class MessageExchange(
    message: Message,
    reply: Message
)