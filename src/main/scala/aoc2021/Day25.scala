package aoc2021

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, ReaderWriterState, State, StateT, ValidatedNec}
import cats.implicits.*
import commons.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

enum Cell:
  case Empty, EastSC, SouthSC

object Day25 extends Solver[Matrix[Cell], Int] :

  import Cell.*

  val puzzle = Puzzle(Year(2021), Day(25))

  private def charToBoolean(c: Char): ValidatedNec[String, Cell] = c match {
    case '.' => Valid(Empty)
    case '>' => Valid(EastSC)
    case 'v' => Valid(SouthSC)
    case _ => Invalid(NonEmptyChain(s"Unexpected character $c"))
  }

  override def parser(input: String): ValidatedNec[String, Matrix[Cell]] =
    input.linesIterator.toList.traverse(_.toList.traverse(charToBoolean))

  type RWS[A] = ReaderWriterState[Unit, Int, Matrix[Cell], A]

  def shiftRow(row: List[Cell], toShift: Cell): List[Cell] =
    @tailrec
    def rec(toGo: List[Cell], acc: List[Cell]): List[Cell] =
      if toGo.isEmpty then acc
      else
        toGo.span(_ != Empty) match {
          case (Nil, _) =>
            val (empty, rest) = toGo.span(_ == Empty)
            rec(rest, acc ::: empty)

          case (nonEmpty, _ :: rest) if nonEmpty.last == toShift =>
            rec(rest, acc ::: (nonEmpty.init ::: (Empty :: toShift :: Nil)))

          // wrap-around edge case
          case (nonEmpty, Nil) if nonEmpty.last == toShift && row.headOption.contains(Empty) =>
            toShift :: acc.tail ::: (nonEmpty.init :+ Empty)

          case (nonEmpty, rest) =>
            rec(rest, acc ::: nonEmpty)
        }

    rec(row, List.empty)

  // assumes the matrix has been transposed when handling SouthSC
  private def moveForward(cell: EastSC.type | SouthSC.type): RWS[Unit] = ReaderWriterState.modify { matrix =>
    matrix.map { row => shiftRow(row, cell) }
  }

  val moveEastSC: RWS[Unit] = moveForward(EastSC)
  val moveSouthSC: RWS[Unit] = for {
    _ <- ReaderWriterState.modify[Unit, Int, Matrix[Cell]](_.transpose)
    _ <- moveForward(SouthSC)
    _ <- ReaderWriterState.modify[Unit, Int, Matrix[Cell]](_.transpose)
  } yield ()

  val stepRunner: RWS[Boolean] = for {
    matrix <- ReaderWriterState.get
    _ <- moveEastSC
    _ <- moveSouthSC
    newMatrix <- ReaderWriterState.get
    _ <- ReaderWriterState.tell(1)
  } yield matrix != newMatrix

  override def part1(matrix: Matrix[Cell]): Option[Int] =
    Some(stepRunner.iterateWhile(identity).runL((), matrix).value)

  def matrixToString(matrix: Matrix[Cell]): String =
    matrix.map {
      _.map {
        case Empty => '.'
        case EastSC => '>'
        case SouthSC => 'v'
      }.mkString
    }.mkString("\n")
