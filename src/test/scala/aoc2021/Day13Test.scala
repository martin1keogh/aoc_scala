package aoc2021

import commons.*
import org.scalatest.wordspec.AnyWordSpec

class Day13Test extends AnyWordSpec with PuzzleSolverBehaviour:
  val solver = Day13
  val testCases = List(
    TestCase(
      """6,10
        |0,14
        |9,10
        |0,3
        |10,4
        |4,11
        |6,0
        |6,12
        |4,1
        |0,13
        |10,12
        |3,4
        |3,0
        |8,4
        |1,10
        |2,14
        |8,10
        |9,0
        |
        |fold along y=7
        |fold along x=5""".stripMargin,
      Some(17), None
    ),
  )

  "Day13 Solver".should(behave.like(puzzleSolver(solver, testCases)))
