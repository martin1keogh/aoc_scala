package aoc2021

import commons.*
import org.scalatest.wordspec.AnyWordSpec

class Day25Test extends AnyWordSpec with PuzzleSolverBehaviour :

  val solver = Day25
  val testCases = List(
    TestCase(
      """v...>>.vv>
        |.vv>>.vv..
        |>>.>v>...v
        |>>v>>.>.v.
        |v>v.vv.v..
        |>.>>..v...
        |.vv..>.>v.
        |v.v..>>v.v
        |....v..v.>""".stripMargin,
      Some(58), None
    ),
  )

  "Day25 Solver".should(behave.like(puzzleSolver(solver, testCases)))

  "runStep" should {
    "shift cells correctly" in {
      val initStr =
        """v...>>.vv>
          |.vv>>.vv..
          |>>.>v>...v
          |>>v>>.>.v.
          |v>v.vv.v..
          |>.>>..v...
          |.vv..>.>v.
          |v.v..>>v.v
          |....v..v.>""".stripMargin

      val init = Day25.parser(initStr).toOption.get

      val step1 = Day25.stepRunner.runS((), init).value
      val step2 = Day25.stepRunner.runS((), step1).value

      assert(Day25.matrixToString(step1) ===
        """....>.>v.>
          |v.v>.>v.v.
          |>v>>..>v..
          |>>v>v>.>.v
          |.>v.v...v.
          |v>>.>vvv..
          |..v...>>..
          |vv...>>vv.
          |>.v.v..v.v""".stripMargin)
      assert(Day25.matrixToString(step2) ===
        """>.v.v>>..v
          |v.v.>>vv..
          |>v>.>.>.v.
          |>>v>v.>v>.
          |.>..v....v
          |.>v>>.v.v.
          |v....v>v>.
          |.vv..>>v..
          |v>.....vv.""".stripMargin
      )
    }
  }
