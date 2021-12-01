package aoc2020

import cats.data.Validated
import cats.implicits.*
import commons.*

object Day1 extends LineBasedInput[Int], Solver[List[Int], Int] :
  val TARGET = 2020
  val puzzle = Puzzle(Year(2020), Day(1))

  def parseLine(line: String): Validated[String, Int] =
    line.toIntOption.toValid(s"$line is not an int")

  def part1(input: List[Int]): Option[Int] =
    input.combinations(2).collectFirst {
      case comb if comb.sum == TARGET => comb.product
    }
