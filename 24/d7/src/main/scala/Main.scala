package aoc

import scala.io.Source

def isSatisfiable(part2: Boolean)(target: Long, xs: List[Long]): Boolean =
  xs match
    case Nil => target == 0 || target == 1
    case x :: xs =>
      val digits_nb = Math.log10(x + 1).ceil
      val n = Math.pow(10, digits_nb).toLong

      (target >= x && isSatisfiable(part2)(target - x, xs))
      || (target % x == 0 && isSatisfiable(part2)(target / x, xs))
      || (part2 && target % n == x && isSatisfiable(part2)(target / n, xs))

def calibration(part2: Boolean)(l: List[(Long, List[Long])]): Long =
  l.filter(isSatisfiable(part2)).map(_._1).sum

@main def main(): Unit =
  val lines = Source.fromFile("data/input").getLines()
  val l = lines
    .map(s =>
      val l = s.split(Array(' ', ':')).filterNot(_.isEmpty()).map(_.toLong)
      (l.head, l.tail.toList.reverse)
    )
    .toList

  val part1 = calibration(false)(l)
  val part2 = calibration(true)(l)

  println(s"Part1: $part1")
  println(s"Part2: $part2")
