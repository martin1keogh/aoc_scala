package aoc2021

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, ReaderWriterState, ValidatedNec}
import cats.implicits.*
import commons.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

object Day20 extends Solver[(Array[Boolean], Matrix[Boolean]), Int] :
  val puzzle = Puzzle(Year(2021), Day(20))

  private def charToBoolean(c: Char): ValidatedNec[String, Boolean] = c match {
    case '#' => Valid(true)
    case '.' => Valid(false)
    case _ => Invalid(NonEmptyChain(s"Unexpected character $c"))
  }

  override def parser(input: String): ValidatedNec[String, (Array[Boolean], Matrix[Boolean])] =
    val Array(algoString, inputImageString) = input.split("\\R\\R")
    val algo = algoString.toList.traverse(charToBoolean).map(_.toArray)
    val inputImage = inputImageString.linesIterator.toList.traverse(_.toList.traverse(charToBoolean))
    (algo, inputImage).mapN(_ -> _)

  type RWST[A] = ReaderWriterState[Array[Boolean], Unit, (Int, Matrix[Boolean]), A]

  val getPadding: RWST[Boolean] = for {
    algo <- ReaderWriterState.ask
    state <- ReaderWriterState.get
    pad = algo.head && !algo.last && state._1 % 2 == 1
  } yield pad

  def padMatrix(padding: Boolean): RWST[Unit] =
    ReaderWriterState.modify {
      case (count, matrix) => count -> wrapMatrix(matrix).map(_.map(_.getOrElse(padding)))
    }

  val enhance: RWST[Matrix[Boolean]] = ReaderWriterState { case (algo, (count, image)) =>
    val inner = image.tail.dropRight(1).map(_.tail.dropRight(1))
    val newImage =
      inner.zipWithIndex.map { case (row, rowNumber) =>
        row.zipWithIndex.map { case (_, colNumber) =>
          val surroundings =
            for {
              r <- Range.inclusive(rowNumber, rowNumber + 2)
              c <- Range.inclusive(colNumber, colNumber + 2)
            } yield image(r)(c)

          val index = Integer.parseInt(surroundings.map(if _ then 1 else 0).mkString, 2)
          algo(index)
        }
      }
    ((), count + 1 -> newImage, newImage)
  }

  val runStep: RWST[Int] = for {
    padding <- getPadding
    _ <- padMatrix(padding)
    _ <- padMatrix(padding)
    newImage <- enhance
  } yield newImage.map(_.count(identity)).sum

  override def part1(input: (Array[Boolean], Matrix[Boolean])): Option[Int] =
    val twice = for {
      _ <- runStep
      snd <- runStep
    } yield snd

    Some(twice.runA(input._1, (0, input._2)).value)

  override def part2(input: (Array[Boolean], Matrix[Boolean])): Option[Int] =
    val runner = Range(1, 50).foldLeft(runStep) {
      case (state, i) => state.flatMap(_ => runStep)
    }

    Some(runner.runA(input._1, (0, input._2)).value)
