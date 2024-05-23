package tangiblegpt.model

/**
 * Handle an interaction with GPT where the request and response can be arbitrary types
 *
 * @param render Render prompt from request
 * @param responseFormatDescription Specification of response format
 * @param parse Parse response as given response type (None if unparsable)
 * @param objective Objective of interaction. Probably necessary if the request is not a string
 * @tparam A Request type
 * @tparam B Response type
 */
class InteractionHandler[A, B](
    val render: A => String,
    val responseFormatDescription: A => String,
    val parse: (A, String) => Option[B],
    val objective: Option[String] = None
)
