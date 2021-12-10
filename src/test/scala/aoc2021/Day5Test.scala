package aoc2021

import commons.*
import org.scalatest.wordspec.AnyWordSpec

class Day5Test extends AnyWordSpec with PuzzleSolverBehaviour:
  val solver = Day5
  val testCases = List(
    TestCase(
      """0,9 -> 5,9
        |8,0 -> 0,8
        |9,4 -> 3,4
        |2,2 -> 2,1
        |7,0 -> 7,4
        |6,4 -> 2,0
        |0,9 -> 2,9
        |3,4 -> 1,4
        |0,0 -> 8,8
        |5,5 -> 8,2""".stripMargin,
      Some(5), Some(12)
    ),
  )

  "Day5 Solver".should(behave.like(puzzleSolver(solver, testCases)))
