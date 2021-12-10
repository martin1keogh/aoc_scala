package aoc2021

import commons.*
import org.scalatest.wordspec.AnyWordSpec

class Day6Test extends AnyWordSpec with PuzzleSolverBehaviour:
  val solver = Day6
  val testCases = List(
    TestCase(
      """3,4,3,1,2""".stripMargin,
      Some(5934), Some(26984457539L)
    ),
  )

  "Day6 Solver".should(behave.like(puzzleSolver(solver, testCases)))
