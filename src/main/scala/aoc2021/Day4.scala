package aoc2021

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.implicits.*
import commons.*

import scala.annotation.tailrec
import scala.util.Try


object Day4 extends Solver[(List[Int], List[Matrix[Int]]), Int] :
  val puzzle = Puzzle(Year(2021), Day(4))

  def parser(input: String): ValidatedNec[String, (List[Int], List[Matrix[Int]])] =
    input.split("(\\n\\n|\\r\\n\\r\\n)").foldLeft(Option.empty[(List[Int], List[Matrix[Int]])]) {
      case (None, line) =>
        line.split(',').toList.traverse(_.toIntOption.toValid("not an int")).toOption.map((_, List.empty))
      case (Some(draws, charts), group) =>
        val rows = group.split("(\\n|\\r\\n)").toList
        val matrix = rows.map(_.split("\\s+").filter(_ != "").map(_.toInt).toList)
        Some(draws, charts :+ matrix)
    }.toValid(NonEmptyChain("tough"))


  def part1(input: (List[Int], List[Matrix[Int]])): Option[Int] =
    val (draws, grids) = input
    (
      for {
        draw <- draws.inits.toList.reverse
        grid <- grids if isWinningGrid(grid, draw)
        nonWinningNumbers = grid.flatten.filterNot(draw.contains(_))
      } yield nonWinningNumbers.sum * draw.last
    ).headOption

  override def part2(input: (List[Int], List[Matrix[Int]])): Option[Int] =
    val (draws, grids) = input
    (
      for {
        draw <- draws.inits.toList.reverse.tail
        grid <- grids if isWinningGrid(grid, draw) && !isWinningGrid(grid, draw.dropRight(1))
        nonWinningNumbers = grid.flatten.filterNot(draw.contains(_))
      } yield nonWinningNumbers.sum * draw.last
    ).lastOption

  private def isWinningGrid(grid: Matrix[Int], draws: List[Int]): Boolean =
    (grid ++ grid.transpose).exists(_.forall(draws.contains))
