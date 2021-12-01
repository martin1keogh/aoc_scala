package aoc2021

import cats.data.Validated
import cats.implicits.*
import commons.*

object Day1 extends LineBasedInput[Int], Solver[List[Int], Int] :
  val puzzle = Puzzle(Year(2021), Day(1))

  def parseLine(line: String): Validated[String, Int] =
    line.toIntOption.toValid(s"$line is not an int")

  def part1(input: List[Int]): Option[Int] =
    Some(input.sliding(2).count { case List(v1, v2) => v1 < v2 })

  override def part2(input: List[Int]): Option[Int] =
    Some(input.sliding(3).sliding(2).map(_.toList).count { case List(v1, v2) => v1.sum < v2.sum })
