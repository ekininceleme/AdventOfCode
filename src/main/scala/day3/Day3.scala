package day3

import cats.effect.IO
import day3.Day3.Directions.directions
import utils.Utils

import scala.annotation.tailrec

object Day3 {

  case class Position(x: Long, y: Long) {
    def isNextToEnginePart(engineMap: Map[Position, EnginePart]): Boolean = directions.map(_.position).map(_.add(this)).exists(engineMap.contains)

    def add(position: Position): Position = Position(x + position.x, y + position.y)

  }

  sealed trait Direction {
    val position: Position
  }

  object Directions {
    case object North extends Direction {
      val position: Position = Position(0, -1)
    }

    case object South extends Direction {
      val position: Position = Position(0, 1)
    }

    case object West extends Direction {
      val position: Position = Position(-1, 0)
    }

    case object East extends Direction {
      val position: Position = Position(1, 0)
    }

    case object NorthEast extends Direction {
      val position: Position = Position(1, -1)
    }

    case object NorthWest extends Direction {
      val position: Position = Position(-1, -1)
    }

    case object SouthEast extends Direction {
      val position: Position = Position(1, 1)
    }

    case object SouthWest extends Direction {
      val position: Position = Position(-1, 1)
    }

    val directions: Seq[Direction] = Seq(
      North, South, East, West,
      NorthEast, NorthWest, SouthEast, SouthWest
    )
  }

  trait Thing {
    val position: Position
  }

  case class Digit(position: Position, char: Char) extends Thing

  object Digit {
    implicit val digitOrdering: Ordering[Digit] = Ordering.by(d => (d.position.x, d.position.y))
  }

  case class EnginePart(position: Position, char: Char) extends Thing {
    def getAdjacentNumbers(numbers: List[Number]): List[Number] = numbers.filter(_.isNextToGear(this)).distinct
  }

  case class Engine(enginePartMap: Map[Position, EnginePart], digitMap: Map[Position, Digit], numbers: List[Number])

  case class Number(digits: List[Digit]) {
    def isNextToEnginePart(enginePartMap: Map[Position, EnginePart]): Boolean =
      digits.map(_.position).exists(_.isNextToEnginePart(enginePartMap))

    def isNextToGear(enginePart: EnginePart): Boolean =
      (for {
        digit <- digits
        direction <- directions
        check = digit.position.add(direction.position)
      } yield check == enginePart.position).exists(identity)

    def getIntegerNumber: Option[Int] = digits.map(_.char).mkString.toIntOption

  }

  def part1(path: String): IO[Int] = {
    getEngine(path).map { engine =>
      engine.numbers.filter(_.isNextToEnginePart(engine.enginePartMap)).flatMap(_.getIntegerNumber).sum
    }
  }

  def part2(path: String): IO[Int] =
    getEngine(path).map { engine =>
      engine.enginePartMap.map {
        case (_, enginePart) => enginePart match {
          case e@EnginePart(_, '*') => e.getAdjacentNumbers(engine.numbers) match {
            case e if e.length == 2 => e.flatMap(_.getIntegerNumber).product
            case _ => 0
          }
          case _ => 0
        }
        case _ => 0
      }.sum
    }

  private def getEngine(path: String) = {
    val engineIo = (for {
      (string, y) <- Utils.readFile(path).zipWithIndex
      (char, x) <- fs2.Stream.emits(string).zipWithIndex
      position = Position(x, y)
      if char != '.'
      thing = char match {
        case d if d.isDigit => Digit(position, d)
        case _ => EnginePart(position, char)
      }
    } yield thing)
      .map(d => (d.position, d))
      .compile
      .to(Map)

    for {
      engine <- engineIo
      digitMap = engine.flatMap {
        case (key, value: Digit) => Some((key, value))
        case _ => None
      }
      enginePartMap = engine.flatMap {
        case (key, value: EnginePart) => Some((key, value))
        case _ => None
      }
      numbers = digitMap.map {
          case (_, digit) => getNumber(digitMap, digit)
        }
        .toList.distinct
    } yield Engine(enginePartMap, digitMap, numbers)
  }

  private def getNumber(m: Map[Position, Digit], d: Digit): Number = {
    @tailrec
    def helper(current: Position, list: List[Digit], direction: Direction): List[Digit] = {
      m.get(current.add(direction.position)) match {
        case None => list
        case Some(v) =>
          helper(v.position, list :+ v, direction)
      }
    }

    val digits: List[Digit] = (helper(d.position, List.empty, Directions.West) :+ d) ++ helper(d.position, List.empty, Directions.East)
    Number(digits.sorted)
  }
}
