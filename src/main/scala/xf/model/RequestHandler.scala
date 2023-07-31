package xf.model

class RequestHandler[A](
    val serialize: A => String
)