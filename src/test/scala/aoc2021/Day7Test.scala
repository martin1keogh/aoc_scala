package aoc2021

import commons.*
import org.scalatest.wordspec.AnyWordSpec

class Day7Test extends AnyWordSpec with PuzzleSolverBehaviour:
  val solver = Day7
  val testCases = List(
    TestCase(
      """16,1,2,0,4,2,7,1,2,14""".stripMargin,
      Some(37), Some(168)
    ),
  )

  "Day6 Solver".should(behave.like(puzzleSolver(solver, testCases)))
