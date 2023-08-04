package xf.model

class InteractionHandler[A, B](
    val objective: String,
    val render: A => String,
    val responseFormatDescription: A => String,
    val parse: (A, String) => Option[B]
)
