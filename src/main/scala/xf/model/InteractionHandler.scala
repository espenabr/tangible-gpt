package xf.model

/**
 * Handle an interaction with GPT where the request and response can be arbitrary types
 *
 * @param objective Objective of interaction. Probably necessary if the request is not a string
 * @param render Render prompt from request
 * @param responseFormatDescription Specification of response format
 * @param parse Parse response as given response type (None if unparsable)
 * @tparam A Request type
 * @tparam B Response type
 */
class InteractionHandler[A, B](
    val objective: String,
    val render: A => String,
    val responseFormatDescription: A => String,
    val parse: (A, String) => Option[B]
)
