package aoc2021

import cats.Traverse
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, State, Validated, ValidatedNec}
import cats.implicits.*
import commons.*

import scala.annotation.tailrec
import scala.util.Try
import scala.util.parsing.combinator.*

object Day21 extends Solver[(Int, Int), Int] :
  val puzzle = Puzzle(Year(2021), Day(21))

  override def parser(input: String): ValidatedNec[String, (Int, Int)] =
    input.linesIterator.toList.traverse { line =>
      line.split(' ').last.toIntOption.toValid(NonEmptyChain("Invalid Int"))
    }.map {
      case first :: second :: Nil => first -> second
      //      case _ => Invalid(NonEmptyChain("Wrong number of elements"))
    }

  val spaces: LazyList[Int] = LazyList.range[Int](1, 11) #::: spaces

  case class GameState(
    player: Int,
    nbRolls: Int,
    pos1: Int,
    score1: Int,
    pos2: Int,
    score2: Int
  )

  override def part1(positions: (Int, Int)): Option[Int] =
    println(spaces.take(30))
    val draws = LazyList.from(0).map(_ % 100 + 1)
    val game = State[(LazyList[Int], GameState), (Int, Int)] {
      case (draws, GameState(player, nbRolls, pos1, score1, pos2, score2)) =>
        val (drawn, next) = draws.splitAt(3)
        val (pos, score) = if player == 0 then (pos1, score1) else (pos2, score2)
        val newPos = spaces((pos - 1) + drawn.sum)
        val newScore = score + newPos
        println(s"Player ${player + 1} moved to $newPos, score is $newScore")
        val newGameState =
          if player == 0 then
            GameState(1, nbRolls + 3, newPos, newScore, pos2, score2)
          else
            GameState(0, nbRolls + 3, pos1, score1, newPos, newScore)
        ((next, newGameState), (newGameState.score1, newGameState.score2))
    }

    val res =
      game
        .iterateUntil { case (score1, score2) =>
          score1 >= 1000 || score2 >= 1000
        }.transform { case ((_, gs), (score1, score2)) =>
          ((), math.min(score1, score2) * gs.nbRolls)
        }.runA(draws, GameState(0, 0, positions._1, 0, positions._2, 0))
         .value

    Some(res)
