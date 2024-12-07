package aoc.part2

import scala.io.Source

def isSatisfiable(target: Long, xs: List[Long]): Boolean =
  xs match
    case Nil => target == 0 || target == 1
    case x :: xs =>
      (if target >= x then isSatisfiable(target - x, xs) else false)
      || (if target % x == 0 then isSatisfiable(target / x, xs) else false)
      || {
        val digits_nb = Math.log10(x + 1).ceil.toLong
        val n = Math.pow(10, digits_nb).toLong
        if target % n == x then isSatisfiable(target / n, xs) else false
      }

def calibration(): Long =
  val lines = Source.fromFile("data/input").getLines()
  val l = lines
    .map(s =>
      val l = s.split(Array(' ', ':')).filterNot(_.isEmpty()).map(_.toLong)
      (l.head, l.tail.toList.reverse)
    )

  l.filter(isSatisfiable).map(_._1).sum
