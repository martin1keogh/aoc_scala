package aoc2021

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.implicits.*
import commons.*

import scala.annotation.tailrec
import scala.util.Try

type Point = (Int, Int)

object Day5 extends LineBasedInput[(Point, Point)], Solver[List[(Point, Point)], Int] :
  val puzzle = Puzzle(Year(2021), Day(5))

  def parseLine(input: String): Validated[String, (Point, Point)] =
    val regex = raw"(\d+),(\d+) -> (\d+),(\d+)".r
    input match {
      case regex(x1, y1, x2, y2) => Valid((x1.toInt, y1.toInt) -> (x2.toInt, y2.toInt))
      case _ => Invalid(s"Match error for $input")
    }

  def part1(input: List[(Point, Point)]): Option[Int] =
    import math.{min, max}

    val allPoints = input.flatMap {
      case ((x1, y1), (x2, y2)) if x1 == x2 =>
        (min(y1, y2) to max(y1, y2)).map(x1 -> _)
      case ((x1, y1), (x2, y2)) if y1 == y2 =>
        (min(x1, x2) to max(x1, x2)).map(_ -> y1)
      case _ => List()
    }
    val atLeastTwoPoints = allPoints.groupBy(identity).count(_._2.length >= 2)
    Some(atLeastTwoPoints)

  override def part2(input: List[(Point, Point)]): Option[Int] =
    import math.{min, max}

    val allPoints = input.flatMap {
      case ((x1, y1), (x2, y2)) if x1 == x2 =>
        (y1 to y2 by step(y1, y2)).map(x1 -> _)
      case ((x1, y1), (x2, y2)) if y1 == y2 =>
        (x1 to x2 by step(x1, x2)).map(_ -> y1)
      case ((x1, y1), (x2, y2)) =>
        (x1 to x2 by step(x1, x2)) zip (y1 to y2 by step(y1, y2))
    }
    val atLeastTwoPoints = allPoints.groupBy(identity).count(_._2.length >= 2)
    Some(atLeastTwoPoints)

  def step(p1: Int, p2: Int): Int =
    if (p1 > p2) -1 else 1
