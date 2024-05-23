package xf.model

import xf.gpt.GptApiClient.Common.Message

enum FailedInteraction:
  case FailedRequest(errorMessage: String)
  case ParseError(rawMessage: String, history: List[Message])
