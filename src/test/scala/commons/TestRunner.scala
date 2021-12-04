package commons

import cats.data.Validated
import commons.Solver
import org.scalatest.Inspectors.forAll
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.wordspec.AnyWordSpec


case class TestCase(
  input: String,
  answerPart1: Option[_],
  answerPart2: Option[_],
) {
  def answerFor(i: 1 | 2) = i match {
    case 1 => answerPart1
    case 2 => answerPart2
  }
}

trait PuzzleSolverBehaviour:
  self: AnyWordSpec =>

  def puzzleSolver[I, O](solver: Solver[I, O], testCases: List[TestCase]): Unit = {
    s"AoC ${solver.puzzle.year} - ${solver.puzzle.day}" should {
      List[1 | 2](1, 2) foreach { part =>
        s"for part $part" should {
          testCases.zipWithIndex.collect {
            case (tc, i) if tc.answerFor(part).isDefined =>
              val lines = tc.input

              tc.answerFor(part) map { answer =>
                s"parse example ${i + 1}" in {
                  solver.parser(lines) should beValid
                }

                s"return $answer for example ${i + 1}" in {
                  val meth = part match {
                    case 1 => solver.part1
                    case 2 => solver.part2
                  }
                  meth(solver.parser(lines).toOption.get) should contain(answer)
                }
              }
          }
        }
      }
    }

    class ValidatedMatcher extends Matcher[Validated[_, _]] {
      override def apply(v: Validated[_, _]): MatchResult =
        MatchResult(
          v.isValid,
          s"$v is not Valid",
          s"$v is Valid",
        )
    }

    def beValid = new ValidatedMatcher()
  }
