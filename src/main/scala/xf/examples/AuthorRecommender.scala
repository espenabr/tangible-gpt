package xf.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import xf.examples.Common.{clientResource, createConversationClient, extractKey}
import xf.Input.{collectSelectedItems, prompt}
import xf.interactionhandlers.RecommendAuthors.{
  recommendedAuthorsBasedOnFavoriteStrengths,
  strengthsBasedOnAuthorsHandler,
  FavoriteAuthors,
  FavoriteStrengths
}

import scala.util.Random

object AuthorRecommender extends IOApp {

  /*
   * Flow:
   *  1. Users inputs favorite authors
   *  2. GPT returns a list of unique strengths of these authors combined
   *  3. Users selects the most prefered of these strengths
   *  4. GPT returns a list of recommended authors based on the conversation
   */

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val interactions = createConversationClient(client, extractKey(args))
      for {
        input              <- prompt("List authors that you like, separated by comma")
        authors             = FavoriteAuthors(input.split(",").toList.map(_.strip()))
        strengthsOfAuthors <- interactions.chat(authors, strengthsBasedOnAuthorsHandler)
        favoriteStrengths  <- collectSelectedItems(strengthsOfAuthors.value.get.strengths)
        recommended        <- interactions.chat(
                                FavoriteStrengths(Random.shuffle(favoriteStrengths)),
                                recommendedAuthorsBasedOnFavoriteStrengths,
                                strengthsOfAuthors.history
                              )
        _                  <- Console[IO].println(recommended.value.get)
      } yield ExitCode.Success
    }

}
