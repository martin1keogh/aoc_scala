package aoc2021

import commons.*
import org.scalatest.wordspec.AnyWordSpec

class Day21Test extends AnyWordSpec with PuzzleSolverBehaviour :

  val solver = Day21
  val testCases = List(
    TestCase(
      """Player 1 starting position: 4
        |Player 2 starting position: 8""".stripMargin,
      Some(739785), None
    ),
  )

  "Day21 Solver".should(behave.like(puzzleSolver(solver, testCases)))
