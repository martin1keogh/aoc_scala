package aoc2021

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*
import commons.*

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.util.Try

type Matrix = List[List[Int]]

object Day3 extends LineBasedInput[List[Int]], Solver[Matrix, Int] :
  val puzzle = Puzzle(Year(2021), Day(3))

  def parseLine(line: String): Validated[String, List[Int]] =
    line.split("").toList.traverse {
      case b if b == "0" || b == "1" => Valid(b.toInt)
      case nb => Invalid(nb)
    }

  def part1(input: Matrix): Option[Int] =
    val gammaBitList :: epsilonBitList :: Nil =
      input
        .transpose
        .map(sortByFrequency)
        .transpose

    Some(bitsToInt(gammaBitList) * bitsToInt(epsilonBitList))

  override def part2(input: Matrix): Option[Int] =
    Some(ratingCalculator(input, 0) * ratingCalculator(input, 1))

  private def bitsToInt(bits: List[Int]): Int =
    Integer.parseInt(bits.mkString, 2)

  private def sortByFrequency(bits: List[Int]): List[Int] =
    bits.groupMapReduce(identity)(_ => 1)(_ + _)
      .toList
      .sortBy(_.swap)  // not reflected by the name, but this will sort by frequency & put 1s last in case of a tie (useful in part2)
      .map(_._1)

  @tailrec
  private def ratingCalculator(input: Matrix, leastOrMost: 0 | 1, rank: Int = 0): Int = input match {
    case Nil => ???
    case hd :: Nil => bitsToInt(hd)
    case _ =>
      val target = sortByFrequency(input.map(_ (rank)))(leastOrMost)
      val toConsider = input.filter(_ (rank) == target)
      ratingCalculator(toConsider, leastOrMost, rank + 1)
  }
