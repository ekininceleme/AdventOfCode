package day2

import cats.effect.IO
import fs2.Pipe
import utils.Utils

object Day2 {

  private val roundRegex = "(.+?);".r
  private val gameRegex = "Game (\\d+): (.+)".r


  private def roundParser(str: String) =
    roundRegex.findAllIn(str).map(round => Round(
      red = "(\\d+) red".r.findFirstMatchIn(round).map(_.group(1)).flatMap(_.toIntOption).getOrElse(0),
      green = "(\\d+) green".r.findFirstMatchIn(round).map(_.group(1)).flatMap(_.toIntOption).getOrElse(0),
      blue = "(\\d+) blue".r.findFirstMatchIn(round).map(_.group(1)).flatMap(_.toIntOption).getOrElse(0),
    )).toList

  private def gameParser(str: String) = {
    str match {
      case gameRegex(gameId, rounds) => gameId.toIntOption.map(Game(_, roundParser(rounds + ";")))
      case _ => None
    }
  }

  case class Round(green: Int = 0, red: Int = 0, blue: Int = 0) {
    def isPossible(bag: Bag): Boolean = bag.red >= red && bag.blue >= blue && bag.green >= green

    def minimumRound(round: Round): Round = Round(
      green = math.max(round.green, green),
      red = math.max(round.red, red),
      blue = math.max(round.blue, blue)
    )

    def power: Int = green * red * blue
  }

  case class Game(id: Int, rounds: List[Round]) {
    def isPossible(bag: Bag): Boolean = rounds.forall(_.isPossible(bag))

    def minimumRound: Round = rounds.fold(Round())(_.minimumRound(_))
  }

  case class Bag(green: Int, red: Int, blue: Int)

  def part1(path: String, bag: Bag) =
    (Utils.readFile(path) through getGames)
      .evalTap(IO.println(_))
      .filter(_.isPossible(bag))
      .evalTap(b => IO.println(b.isPossible(bag)))
      .map(_.id)
      .compile
      .fold(0)(_ + _)

  def part2(path: String) =
    (Utils.readFile(path) through getGames)
      .map(_.minimumRound)
      .map(_.power)
      .compile
      .fold(0)(_ + _)

  private def getGames: Pipe[IO, String, Game] = _.map(gameParser)
    .flatMap(fs2.Stream.fromOption(_))

}
