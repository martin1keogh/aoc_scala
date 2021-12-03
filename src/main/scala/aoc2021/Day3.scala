package aoc2021

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*
import commons.*

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
    Some(oxygenRating(input) * co2Rating(input))

  private def bitsToInt(bits: List[Int]): Int =
    Integer.parseInt(bits.mkString, 2)

  private def sortByFrequency(bits: List[Int]): List[Int] =
    bits.groupMapReduce(identity)(_ => 1)(_ + _)
      .toList
      .sortBy(_.swap)  // not reflected by the name, but this will sort by frequency & put 1s last in case of a tie (useful in part2)
      .map(_._1)

  private def oxygenRating(input: Matrix, rank: Int = 0): Int = input match {
    case Nil => ???
    case hd :: Nil => println(hd); bitsToInt(hd)
    case _ =>
      val mostFrequent = sortByFrequency(input.map(_ (rank)))(1)
      val toConsider = input.filter(_ (rank) == mostFrequent)
      oxygenRating(toConsider, rank + 1)
  }

  private def co2Rating(input: Matrix, rank: Int = 0): Int = input match {
    case Nil => ???
    case hd :: Nil => println(hd) ; bitsToInt(hd)
    case _ =>
      val leastFrequent = sortByFrequency(input.map(_ (rank)))(0)
      val toConsider = input.filter(_ (rank) == leastFrequent)
      co2Rating(toConsider, rank + 1)
  }
