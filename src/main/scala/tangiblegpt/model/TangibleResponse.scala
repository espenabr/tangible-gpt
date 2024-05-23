package tangiblegpt.model

import tangiblegpt.gpt.GptApiClient.Common.Message

case class TangibleResponse[T](
    value: T,
    rawMessage: String,
    history: List[Message]
):
  def printHistory() =
    val zz: List[String] = history.map { h =>
      s"""<${h.role}>
         |
         |${h.content}""".stripMargin
    }
    println(zz.mkString("\n-----------------------------------------------------------------\n"))
    println("-----------------------------------------------------------------")
    println(
      s"""<Last message>
         |
         |$rawMessage""".stripMargin
    )
