package aoc2021

import commons.*
import org.scalatest.wordspec.AnyWordSpec

class Day14Test extends AnyWordSpec with PuzzleSolverBehaviour:
  val solver = Day14
  val testCases = List(
    TestCase(
      """NNCB
        |
        |CH -> B
        |HH -> N
        |CB -> H
        |NH -> C
        |HB -> C
        |HC -> B
        |HN -> C
        |NN -> C
        |BH -> H
        |NC -> B
        |NB -> B
        |BN -> B
        |BB -> N
        |BC -> B
        |CC -> N
        |CN -> C""".stripMargin,
      Some(1588), Some(2188189693529L)
    ),
  )

  "Day14 Solver".should(behave.like(puzzleSolver(solver, testCases)))
