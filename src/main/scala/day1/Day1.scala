package day1

import cats.effect.IO
import fs2.Pipe
import fs2.io.file.{Files, Path}

import scala.util.Try
import scala.util.matching.Regex

object Day1 {

  private val replacements = Map("one" -> "1", "two" -> "2", "three" -> "3", "four" -> "4", "five" -> "5", "six" -> "6", "seven" -> "7", "eight" -> "8", "nine" -> "9")
  private val regex: String = replacements.keys.map(Regex.quote).mkString("|")
  val s = s"(?=(\\d|$regex))"

  def part1(path: String): IO[Int] =
    (readFile(path) through removeNonDigits through toInt).compile.fold(0)(_ + _)

  def part2(path: String): IO[Int] =
    (readFile(path) through extractWords through wordsToDigits through toInt).compile.fold(0)(_ + _)

  private val wordsToDigits: Pipe[IO, String, String] = _.map(regex.r.replaceAllIn(_, m => replacements(m.matched)))

  private val toInt: Pipe[IO, String, Int] = _.map(str => str.headOption.map(_.toString).getOrElse("") + str.lastOption.map(_.toString).getOrElse(""))
    .flatMap(c => fs2.Stream.fromOption(Try(c.toInt).toOption))

  private val removeNonDigits: Pipe[IO, String, String] = _.map(_.replaceAll("\\D", ""))

  private val extractWords: Pipe[IO, String, String] = _.map(s.r.findAllMatchIn(_).map(_.group(1)).mkString)

  private def readFile(filePath: String): fs2.Stream[IO, String] =
    Files[IO].readUtf8Lines(Path(filePath))
}
