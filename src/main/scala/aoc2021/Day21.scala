package aoc2021

import cats.arrow.FunctionK
import cats.data.*
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*
import cats.{Eval, Id, Traverse, ~>}
import commons.*

import scala.annotation.tailrec
import scala.collection.mutable.Map as MMap
import scala.util.Try
import scala.util.parsing.combinator.*

object Day21 extends Solver[(Int, Int), Long] :
  val puzzle = Puzzle(Year(2021), Day(21))

  override def parser(input: String): ValidatedNec[String, (Int, Int)] =
    input.linesIterator.toList.traverse { line =>
      line.split(' ').last.toIntOption.toValid(NonEmptyChain("Invalid Int"))
    }.map {
      case first :: second :: Nil => first -> second
    }

  val spaces: LazyList[Int] = LazyList.range[Int](1, 11) #::: spaces

  def initialGameState(positions: (Int, Int)) = GameState(0, 0, positions._1, 0, positions._2, 0)

  case class GameState(
    player: Int,
    nbRolls: Int,
    pos1: Int,
    score1: Int,
    pos2: Int,
    score2: Int
  )

  val game: State[(LazyList[Int], GameState), (Int, Int)] = for {
    state <- State.get[(LazyList[Int], GameState)]
    (draws, GameState(player, nbRolls, pos1, score1, pos2, score2)) = state
    (drawn, next) = draws.splitAt(3)
    (pos, score) = if player == 0 then (pos1, score1) else (pos2, score2)
    newPos = spaces((pos - 1) + drawn.sum)
    newScore = score + newPos
    newGameState =
      if player == 0 then GameState(1, nbRolls + 3, newPos, newScore, pos2, score2)
      else GameState(0, nbRolls + 3, pos1, score1, newPos, newScore)
    _ <- State.set((next, newGameState))
  } yield (newGameState.score1, newGameState.score2)

  override def part1(positions: (Int, Int)): Option[Long] =
    val draws = LazyList.from(0).map(_ % 100 + 1)

    val res =
      game
        .iterateUntil { case (score1, score2) =>
          score1 >= 1000 || score2 >= 1000
        }
        .transform { case ((_, gs), (score1, score2)) =>
          ((), math.min(score1, score2) * gs.nbRolls)
        }
        .runA(draws, initialGameState(positions))
        .value

    Some(res)

  override def part2(positions: (Int, Int)): Option[Long] =
    val draws = for {
      x <- List(1, 2, 3)
      y <- List(1, 2, 3)
      z <- List(1, 2, 3)
    } yield List(x, y, z)

    val weightedDraws: LazyList[(Int, Int)] = draws.groupBy(_.sum).view.mapValues {
      case Nil => ???
      case combinations => combinations.length
    }.toList.sortBy(_._1).to(LazyList)

    type Move = Int
    type BranchWeight = Long
    type ST[A] = StateT[LazyList, (Move, BranchWeight, GameState), A]

    val target = 21

    // XXX how to derive this from `game`?
    val gameP2: ST[(Int, Int)] = for {
      state <- StateT.get
      (move, branchWeight, gs@GameState(player, _, pos1, score1, pos2, score2)) = state
      newGameState = {
        val (pos, score) = if player == 0 then (pos1, score1) else (pos2, score2)
        val newPos = if pos + move <= 10 then pos + move else pos + move - 10
        val newScore = score + newPos

        if move == 0 then gs // init hack
        else if player == 0 then gs.copy(player=1, pos1=newPos, score1=newScore)
        else gs.copy(player=0, pos2=newPos, score2=newScore)
      }
      _ <- if newGameState.score1 >= target || newGameState.score2 >= target then StateT.setF(LazyList(state))
           else StateT.setF(weightedDraws.map { case (draw, weight) => (draw, branchWeight * weight, newGameState) })
    } yield (newGameState.score1, newGameState.score2)

    val (wins1, wins2): (Long, Long) =
      gameP2
        .iterateUntil { case (score1, score2) => score1 >= target || score2 >= target }
        .run((0, 1L, initialGameState(positions)))
        .foldLeft((0L, 0L)) { case ((wins1, wins2), ((_, weight, _), (score1, score2))) =>
          if score1 > score2 then (wins1 + weight, wins2) else (wins1, wins2 + weight)
        }

    Some(math.max(wins1, wins2))
