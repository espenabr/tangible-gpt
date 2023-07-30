package xf

import xf.model.Table.Column
import xf.model.Table.Column.{BooleanColumn, NumberColumn, SingleChoiceColumn, TextColumn}

object Prompting {

  // Specification of response format

  def itemsWithAlternativesResponse(noOfItems: Option[Int], noOfAlternatives: Option[Int]): String = {
    val example = noOfAlternatives match
      case Some(n) =>
        s"""<item1>;${(1 to n).map(n => s"<option$n>").mkString(";")}
           |<item2>;${(1 to n).map(n => s"<option$n>").mkString(";")}
           |...""".stripMargin
      case None    =>
        s"""<item1>;<option1>;<option2>;<option3>
           |<item2>;<option1>;<option2>;<option3>
           |...""".stripMargin

    s"""The response must be a list of items, one line each.
       |Each item (line) should start with the item, continuing with a list of options, all separated ; (semicolon).
       |Each line should not be prefixed with a number or any other character to indicate it's an item in a list, only the item itself and the options.
       |${noOfItems.map(n => s"There must be $n items (lines) in total.").getOrElse("")}
       |${noOfAlternatives.map(n => s"Each item should have $n options each.").getOrElse("")}
       |
       |Example:
       |$example
       |
       |I want nothing else in the response except these items.""".stripMargin
  }

  // Specification of questions from GPT

  val yesNoQuestionsOnly = "All questions should yes- or no- answers."

  val numericQuestionsOnly = "All numbers should have a number as an answer."

}
