package aoc2021

import cats.Traverse
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.implicits.*
import commons.*

import scala.annotation.tailrec
import scala.util.Try

object Day9 extends Solver[Matrix[Int], Int] :
  val puzzle = Puzzle(Year(2021), Day(9))

  override def parser(input: String): ValidatedNec[String, Matrix[Int]] =
    input.linesIterator.toList.traverse { line =>
      line.toList.traverse {
        // a bit annoying that `toIntOption` does not exist on `Char`
        _.toString.toIntOption.toValid(NonEmptyChain("Not an number"))
      }
    }

  override def part1(matrix: Matrix[Int]): Option[Int] =
    val value = wrapMatrix(matrix)
    val res =
      Traverse[List].sliding3(value).flatMap {
        case (above, middle, below) =>
          (above zip middle zip below).sliding3.flatMap {
            case (((_, up), _), ((left, Some(elem)), down), ((_, right), _)) =>
              if List(up, down, left, right).flatten.forall(elem < _) then
                Some(elem + 1)
              else None
          }
      }.sum
    Some(res)
