package aoc2021

import cats.data.Validated
import cats.data.Validated.Invalid
import cats.implicits.*
import commons.*

import scala.util.Try

enum Direction:
  case Forward, Down, Up

case class Command(direction: Direction, value: Int)

object Day2 extends LineBasedInput[Command], Solver[List[Command], Int] :
  val puzzle = Puzzle(Year(2021), Day(2))

  def parseLine(line: String): Validated[String, Command] =
    line.split(' ') match {
      case Array(directionStr, valueStr) =>
        val direction = Validated.catchOnly[IllegalArgumentException](Direction.valueOf(directionStr.capitalize)).leftMap(_ => s"Invalid direction $directionStr")
        val value = Validated.catchOnly[NumberFormatException](valueStr.toInt).leftMap(_ => s"Invalid number $valueStr")
        (direction, value).mapN(Command.apply)
      case _ => Invalid(s"Wrong number of word in string $line")
    }

  def part1(input: List[Command]): Option[Int] =
    val (finalDepth, finalHorizontalPositon) =
      input.foldLeft((0, 0)) { case ((depth, horizontalPosition), command) => command match {
        case Command(Direction.Forward, value) => (depth, horizontalPosition + value)
        case Command(Direction.Down, value) => (depth + value, horizontalPosition)
        case Command(Direction.Up, value) => (depth - value, horizontalPosition)
      }}
    Some(finalDepth * finalHorizontalPositon)

  override def part2(input: List[Command]): Option[Int] =
    val (finalDepth, finalHorizontalPositon, _) =
      input.foldLeft((0, 0, 0)) { case ((depth, horizontalPosition, aim), command) => command match {
        case Command(Direction.Forward, value) => (depth + value * aim, horizontalPosition + value, aim)
        case Command(Direction.Down, value) => (depth, horizontalPosition, aim + value)
        case Command(Direction.Up, value) => (depth, horizontalPosition, aim - value)
      }}
    Some(finalDepth * finalHorizontalPositon)
