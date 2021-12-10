package aoc2021

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.implicits.*
import commons.*

import scala.annotation.tailrec
import scala.util.Try

object Day7 extends Solver[List[Int], Int] :
  val puzzle = Puzzle(Year(2021), Day(7))

  def parser(input: String): ValidatedNec[String, List[Int]] =
    input.split(',').toList.traverse { s => s.toIntOption.toValid(NonEmptyChain.of("Not an int")) }

  def part1(crabs: List[Int]): Option[Int] =
    (crabs.min to crabs.max).map(alignmentCost(_, crabs)).minOption

  override def part2(crabs: List[Int]): Option[Int] =
    (crabs.min to crabs.max).map(incrementalAlignmentCost(_, crabs)).minOption

  def alignmentCost(i: Int, crabs: List[Int]): Int =
    crabs.foldMap(c => math.abs(c - i))

  def incrementalAlignmentCost(i: Int, crabs: List[Int]): Int =
    crabs.foldMap { c =>
      val diff = math.abs(c - i)
      (diff * (diff+1)) / 2
    }
