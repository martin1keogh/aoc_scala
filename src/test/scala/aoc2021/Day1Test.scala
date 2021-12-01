package aoc2021

import org.scalatest.wordspec.AnyWordSpec

import commons.*

class Day1Test extends AnyWordSpec with PuzzleSolverBehaviour:
  val solver = Day1
  val testCases = List(
    TestCase(
      """199
        |200
        |208
        |210
        |200
        |207
        |240
        |269
        |260
        |263""".stripMargin,
      Some(7), Some(5)
    ),
    TestCase(
      """1
        |2
        |3""".stripMargin,
      Some(2), None
    ),
  )

  "Day1 Solver".should(behave.like( puzzleSolver(solver, testCases)))
