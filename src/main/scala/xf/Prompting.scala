package xf

import xf.Interactions.Model.Column
import xf.Interactions.Model.Column.*

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

  def specifyListFormat(noOfItems: Option[Int]): String = {
    val example = noOfItems match
      case Some(n) => (1 to n).map(n => s"<item$n>").mkString("\n")
      case None    => "<item1>\n<item2>\n<item3>\n..."
    end example

    s"""The response must be a list of items, one on each line.
       |
       |Example:
       |$example
       |
       |I want nothing else in the response except these items.
       |Also, I do not want any prefix numbers or other characters.""".stripMargin
  }

  def specifyTableFormat(columns: List[Column]): String = {
    def describeColumn(c: Column) = c match {
      case BooleanColumn(name)               => s"$name: Boolean (true or false)"
      case NumberColumn(name)                => s"$name: Number (any number including decimal)"
      case TextColumn(name)                  => s"$name: String"
      case SingleChoiceColumn(name, options) => s"$name: One of the following values: ${options.mkString(", ")}"
    }

    def example(c: Column) = c match {
      case BooleanColumn(_)               => "false"
      case NumberColumn(_)                => "4216"
      case TextColumn(_)                  => "Gordon Blue"
      case SingleChoiceColumn(_, options) => options.head
    }

    s"""I need a table in csv format, using ; (semicolon as a separator. One row per line.
       |
       |${columns.map(describeColumn).mkString("\n")}
       |
       |Example:
       |${columns.map(example).mkString(";")}
       |
       |I do not want any header row, and I want nothing else in the response except the table rows.""".stripMargin
  }

  val specifyBooleanFormat = """I only want a single "yes" or "no" answer. Nothing else."""

  val specifyNumberFormat = "I only want a single number in the response. Nothing else."

  // Specification of questions from GPT

  val yesNoQuestionsOnly = "All questions should yes- or no- answers."

  val numericQuestionsOnly = "All numbers should have a number as an answer."

}
