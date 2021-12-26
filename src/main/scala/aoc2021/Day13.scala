package aoc2021

import cats.Traverse
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.implicits.*
import commons.*

import scala.annotation.tailrec
import scala.util.Try

enum Fold:
  case Horizontal(y: Int)
  case Vertical(x: Int)

object Day13 extends Solver[(Set[(Int, Int)], List[Fold]), Int] :
  val puzzle = Puzzle(Year(2021), Day(13))

  override def parser(input: String): ValidatedNec[String, (Set[(Int, Int)], List[Fold])] =
    val Array(pointsStr, foldsStr) = input.split("\\R\\R")
    val points = pointsStr.linesIterator.toList.map { line =>
      line.split(',') match {
        case Array(x, y) => (x.toIntOption, y.toIntOption).mapN(_ -> _).toValid(NonEmptyChain(s"Invalid ints in $line"))
        case _ => Invalid(NonEmptyChain(s"Unable to read point from $line"))
      }
    }.sequence
    val folds = foldsStr.linesIterator.toList.map { line =>
      line.split(' ') match {
        case Array("fold", "along", foldDef) => foldDef.split("=") match {
          case Array("x", i) => i.toIntOption.toValid(NonEmptyChain("Invalid int $i")).map(Fold.Vertical.apply)
          case Array("y", i) => i.toIntOption.toValid(NonEmptyChain("Invalid int $i")).map(Fold.Horizontal.apply)
          case _ => Invalid(NonEmptyChain(s"Unable to read fold from $line"))
        }
      }
    }.sequence

    (points, folds).mapN(_.toSet -> _)

  override def part1(input: (Set[(Int, Int)], List[Fold])): Option[Int] =
    val (points, firstFold :: _) = input
    val folded = points.map(transformation(firstFold))
    Some(folded.size)

  override def part2(input: (Set[(Int, Int)], List[Fold])): Option[Int] =
    val (points, folds) = input
    val folded = folds.foldLeft(points) { case (points, fold) => points.map(transformation(fold)) }
    val toDraw = List.tabulate(folded.map(_._1).max + 1, folded.map(_._2).max + 1) { case (x, y) => if folded(x, y) then '█' else ' ' }
    toDraw.transpose.foreach(row => println(row.mkString))
    None

  private def transformation(fold: Fold): Function[(Int, Int), (Int, Int)] = fold match {
    case Fold.Horizontal(fy) => {
      case (x, y) => (x, fy - math.abs(y - fy))
    }
    case Fold.Vertical(fx) => {
      case (x, y) => (fx - math.abs(x - fx), y)
    }
  }
