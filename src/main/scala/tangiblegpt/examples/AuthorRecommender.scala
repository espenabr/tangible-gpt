package tangiblegpt.examples

import cats.effect.{IO, IOApp}
import cats.effect.std.Console
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}
import tangiblegpt.Input.{collectSelectedItems, prompt}

object AuthorRecommender extends IOApp.Simple:

  def run: IO[Unit] = clientResource
    .use { client =>
      val ic = createTangibleClient(client, extractKey())
      for {
        input              <- prompt("List authors that you like, separated by comma")
        authors             = input.split(",").toList.map(_.strip())
        strengthsOfAuthors <- ic.expectJson(
                                s"""Given my favorite authors: ${authors.mkString(", ")}
                                   |give me a list of their strengths as authors in a single list""".stripMargin,
                                List("strength1", "strength2", "strengthN")
                              )
        _                  <- Console[IO].println(s"""Select your favourite author strengths""".stripMargin)
        favoriteStrengths  <- collectSelectedItems(strengthsOfAuthors.map(_.value).getOrElse(List.empty))
        recommended        <-
          ic.expectPlainText(
            s"""My favorite strengths are: ${favoriteStrengths.mkString(", ")}
               |Based on these preferred strengths and my favorite authors, I want you to recommend other authors I might like, and also tell me why.""".stripMargin,
            history = strengthsOfAuthors.map(_.history).getOrElse(List.empty)
          )
        _                  <- Console[IO].println(recommended.value)
      } yield ()
    }
