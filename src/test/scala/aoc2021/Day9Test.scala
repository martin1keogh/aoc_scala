package aoc2021

import commons.*
import org.scalatest.wordspec.AnyWordSpec

class Day9Test extends AnyWordSpec with PuzzleSolverBehaviour:
  val solver = Day9
  val testCases = List(
    TestCase(
      """2199943210
        |3987894921
        |9856789892
        |8767896789
        |9899965678""".stripMargin,
      Some(15), None
    ),
  )

  "Day9 Solver".should(behave.like(puzzleSolver(solver, testCases)))
