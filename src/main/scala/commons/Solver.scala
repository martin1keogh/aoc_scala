package commons

import cats.data.*
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*

import java.nio.file.{Path, Paths}
import scala.io.Source
import scala.util.Try

trait Solver[I, O]:
  def puzzle: Puzzle[I, O]

  protected def part1(i: I): Option[O]

  protected def parser(lines: List[String]): ValidatedNec[String, I]

  private def parsedInput: ValidatedNec[String, I] = parser(dataFetcher.getInput)

  private def dataFetcher: DataFetcher = DataFetcher(puzzle)

  def main(args: Array[String]): Unit =
    parsedInput match
      case Invalid(e) =>
        println(s"Nope, try again. Parse errors:")
        e.toList.foreach(println)
      case Valid(i) =>
        println(s"Solution part 1: ${part1(i)}")


trait LineBasedInput[I]:
  def parseLine(line: String): Validated[String, I]

  def parser(lines: List[String]): ValidatedNec[String, List[I]] =
    lines.zipWithIndex.traverse { case (line, i) =>
      parseLine(line).leftMap(inv => NonEmptyChain(s"Line ${i + 1}: $inv"))
    }
