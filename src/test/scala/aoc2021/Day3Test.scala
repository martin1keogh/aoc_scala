package aoc2021

import commons.*
import org.scalatest.wordspec.AnyWordSpec

class Day3Test extends AnyWordSpec with PuzzleSolverBehaviour:
  val solver = Day3
  val testCases = List(
    TestCase(
      """00100
        |11110
        |10110
        |10111
        |10101
        |01111
        |00111
        |11100
        |10000
        |11001
        |00010
        |01010""".stripMargin,
      Some(198), None
    ),
  )

  "Day3 Solver".should(behave.like(puzzleSolver(solver, testCases)))
