package xf

import scala.util.Try
import scala.util.Using
import scala.io.Source
import cats.effect.std.Console
import cats.effect.IO
import cats.implicits.*
import xf.Interactions.Model.QuestionFromGpt
import xf.Interactions.Model.QuestionFromGpt.{
  BooleanQuestionFromGpt,
  MultipleChoiceQuestionFromGpt,
  NumberQuestionFromGpt,
  SingleChoiceQuestionFromGpt,
  TextQuestionFromGpt
}
import xf.Interactions.Model.AnswerToQuestionFromGpt
import xf.Interactions.Model.AnswerToQuestionFromGpt.{
  AnswerToBooleanQuestionFromGpt,
  AnswerToMultipleChoiceQuestionFromGpt,
  AnswerToNumberQuestionFromGpt,
  AnswerToSingleChoiceQuestionFromGpt,
  AnswerToTextQuestionFromQpt
}

object Input {

  def prompt(prompt: String): IO[String] =
    for {
      _     <- Console[IO].print(s"$prompt> ")
      input <- Console[IO].readLine
    } yield input

  def collectAnswers(questionsFromGpt: List[QuestionFromGpt]): IO[List[AnswerToQuestionFromGpt]] =
    questionsFromGpt.map(collectAnswer).sequence

  private def collectAnswer(questionFromGpt: QuestionFromGpt): IO[AnswerToQuestionFromGpt] = {
    def formatOptions(options: List[String]) =
      (1 to options.length).toList
        .zip(options)
        .map { case (index, option) => s"  $index) $option" }
        .mkString("\n")

    val question = questionFromGpt match {
      case BooleanQuestionFromGpt(_, question)                 => s"$question (y/n)"
      case TextQuestionFromGpt(_, question)                    => question
      case NumberQuestionFromGpt(_, question)                  => s"$question (number)"
      case SingleChoiceQuestionFromGpt(_, question, options)   =>
        s"""$question (select one)
           |${formatOptions(options)}""".stripMargin
      case MultipleChoiceQuestionFromGpt(_, question, options) =>
        s"""$question (select those that apply)
           |${formatOptions(options)}""".stripMargin
    }

    for {
      _      <- Console[IO].print(s"$question\n> ")
      answer <- questionFromGpt match {
                  case q: BooleanQuestionFromGpt        =>
                    Console[IO].readLine.flatMap(requireYesNoInput).map(i => AnswerToBooleanQuestionFromGpt(q, i))
                  case q: TextQuestionFromGpt           =>
                    Console[IO].readLine.map(i => AnswerToTextQuestionFromQpt(q, i))
                  case q: NumberQuestionFromGpt         =>
                    Console[IO].readLine.flatMap(requireNumericInput).map(d => AnswerToNumberQuestionFromGpt(q, d))
                  case q: SingleChoiceQuestionFromGpt   =>
                    Console[IO].readLine
                      .flatMap(l => requireSelectedIndexInput(l, q.options.length))
                      .map(i => AnswerToSingleChoiceQuestionFromGpt(q, i))
                  case q: MultipleChoiceQuestionFromGpt =>
                    Console[IO].readLine
                      .flatMap(l => requireSelectedIndicesInput(l, q.options.length))
                      .map(s => AnswerToMultipleChoiceQuestionFromGpt(q, s))
                }
    } yield answer
  }

  private def requireYesNoInput(input: String): IO[Boolean] =
    input.toLowerCase() match {
      case "y" | "yes" => IO.pure(true)
      case "n" | "no"  => IO.pure(false)
      case _           =>
        for {
          retryInput   <- prompt("Please answer 'y' or 'n'")
          booleanInput <- requireYesNoInput(retryInput)
        } yield booleanInput
    }

  private def requireSelectedIndexInput(input: String, maxIndex: Int): IO[Int] =
    Try(input.toInt).toOption.filter(_ <= maxIndex) match
      case Some(i) => IO.pure(i - 1)
      case None    =>
        for {
          retryInput <- prompt(s"Please select item between 1 and $maxIndex")
          indexInput <- requireSelectedIndexInput(retryInput, maxIndex)
        } yield indexInput
    end match

  private def requireNumericInput(input: String): IO[Double] =
    Try(input.strip().toDouble).toOption match
      case Some(d) => IO.pure(d)
      case None    =>
        for {
          retryInput  <- prompt(s"Please enter a number")
          numberInput <- requireNumericInput(retryInput)
        } yield numberInput
    end match

  private def requireSelectedIndicesInput(input: String, maxIndex: Int): IO[Set[Int]] = {
    val stripped = input.strip()
    if stripped.length === 0 then IO.pure(Set.empty)
    else
      Try { stripped.split("[,\\s]").map(_.strip.toInt).toSet }.toEither match
        case Right(indices) =>
          if indices.forall(_ < maxIndex) then IO.pure(indices)
          else requireSelectedIndicesInput(input, maxIndex)
        case Left(_)        =>
          for {
            retryInput    <- prompt(s"Please enter a list of items separated by comma")
            selectedItems <- requireSelectedIndicesInput(retryInput, maxIndex)
          } yield selectedItems
  }

  def readFileContent(path: String): IO[String] =
    IO { Using(Source.fromFile(path))(_.mkString).getOrElse("") }

}
