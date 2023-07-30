package xf.model

class ResponseHandler[A](
    val parse: String => Option[A],
    val typePrompt: String
)
