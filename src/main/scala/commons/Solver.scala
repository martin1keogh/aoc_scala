package commons

import cats.data.*
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*

import java.nio.file.{Path, Paths}
import scala.io.Source
import scala.util.Try

trait Solver[I, O]:
  def puzzle: Puzzle[I, O]

  def part1(i: I): Option[O]
  def part2(i: I): Option[O] = None

  def parser(input: String): ValidatedNec[String, I]

  def parsedInput: ValidatedNec[String, I] = parser(dataFetcher.getInput)

  def dataFetcher: DataFetcher = DataFetcher(puzzle)

  def main(args: Array[String]): Unit =
    parsedInput match
      case Invalid(e) =>
        println(s"Nope, try again. Parse errors:")
      case Valid(i) =>
        println(s"Solution part 1: ${part1(i)}")
        println(s"Solution part 2: ${part2(i)}")


trait LineBasedInput[I]:
  def parseLine(line: String): Validated[String, I]

  def parser(input: String): ValidatedNec[String, List[I]] =
    input.linesIterator.toList.zipWithIndex.traverse { case (line, i) =>
      parseLine(line).leftMap(inv => NonEmptyChain(s"Line ${i + 1}: $inv"))
    }
