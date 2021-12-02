package aoc2021

import commons.*
import org.scalatest.wordspec.AnyWordSpec

class Day2Test extends AnyWordSpec with PuzzleSolverBehaviour:
  val solver = Day2
  val testCases = List(
    TestCase(
      """forward 5
        |down 5
        |forward 8
        |up 3
        |down 8
        |forward 2""".stripMargin,
      Some(150), Some(900)
    ),
  )

  "Day2 Solver".should(behave.like(puzzleSolver(solver, testCases)))
