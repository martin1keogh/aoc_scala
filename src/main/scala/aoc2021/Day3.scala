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
        .map {
          _.groupMapReduce(identity)(_ => 1)(_ + _)
            .toList
            .sortBy(_._2)
            .map(_._1)
        }
        .transpose

    Some(bitsToInt(gammaBitList) * bitsToInt(epsilonBitList))

  override def part2(input: Matrix): Option[Int] =
    None

  private def bitsToInt(bits: List[Int]): Int =
    Integer.parseInt(bits.mkString, 2)

