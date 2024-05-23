package tangiblegpt.examples

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.std.Console
import tangiblegpt.examples.Common.{clientResource, createTangibleClient, extractKey}
import tangiblegpt.model.InteractionHandler

object CustomInteractionHandler extends IOApp:

  /* Interaction handlers can be written on the fly for specific purposes */

  def run(args: List[String]): IO[ExitCode] = clientResource
    .use { client =>
      val tc = createTangibleClient(client, extractKey())
      for
        response <- tc.chat(
                      List("Flyfishing", "Bicycling", "Partying", "Socializing", "Watching TV", "Dishwashing"),
                      new InteractionHandler[List[String], List[(String, Int)]](
                        topics => s"""List of topics:
                         |${topics.map(t => s"- $t").mkString("\n")}""".stripMargin,
                        _ => s"""One topic on each line, along with the public interest in percent (between 0 and 100)
                         |Format: <topic>;<interestPercentage>""".stripMargin,
                        (_, s) =>
                          Some(s.split("\n").toList.map(l => l.split(";").toList).collect { case t :: i :: Nil =>
                            (t, i.toInt)
                          }),
                        objective = Some("Give me the level of public interest in these topics")
                      )
                    )
        _        <- Console[IO].println(response.value.get.map { case (t, i) => s"$t\t$i" }.mkString("\n"))
      yield ExitCode.Success
    }
