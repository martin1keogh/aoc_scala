package aoc2021

import commons.*
import org.scalatest.wordspec.AnyWordSpec

class Day18Test extends AnyWordSpec with PuzzleSolverBehaviour:
  val solver = Day18
  val testCases = List(
    TestCase(
      """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
        |[[[5,[2,8]],4],[5,[[9,9],0]]]
        |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
        |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
        |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
        |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
        |[[[[5,4],[7,7]],8],[[8,3],8]]
        |[[9,3],[[9,9],[6,[4,9]]]]
        |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
        |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".stripMargin,
      Some(4140), None
    ),
  )

  "Day6 Solver".should(behave.like(puzzleSolver(solver, testCases)))

  "explode" should {
    List(
      ("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]"),
      ("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"),
      ("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]"),
      ("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"),
      ("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"),
    ).foreach { case (input, expected) =>
      s"solve $input" in {
        val inputSFN = Day18.parseLine(input).toOption.get
        val expectedSFN = Day18.parseLine(expected).toOption.get

        assert(Day18.explode(inputSFN).get === expectedSFN)
      }
    }
  }

  "split" should {
    List(
      ("[[[[0,7],4],[15,[0,13]]],[1,1]]", "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"),
      ("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]", "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"),
    ).foreach { case (input, expected) =>
      s"solve $input" in {
        val inputSFN = Day18.parseLine(input).toOption.get
        val expectedSFN = Day18.parseLine(expected).toOption.get

        assert(Day18.split(inputSFN).get === expectedSFN)
      }
    }
  }

  "reduceNode should solve" should {
    List(
      ("""[1,1]
         |[2,2]
         |[3,3]
         |[4,4]""".stripMargin, "[[[[1,1],[2,2]],[3,3]],[4,4]]"),
      ("""[1,1]
         |[2,2]
         |[3,3]
         |[4,4]
         |[5,5]""".stripMargin, "[[[[3,0],[5,3]],[4,4]],[5,5]]"),
      ("""[1,1]
         |[2,2]
         |[3,3]
         |[4,4]
         |[5,5]
         |[6,6]""".stripMargin, "[[[[5,0],[7,4]],[5,5]],[6,6]]"),
      ("""[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
         |[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
         |[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
         |[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
         |[7,[5,[[3,8],[1,4]]]]
         |[[2,[2,2]],[8,[8,1]]]
         |[2,9]
         |[1,[[[9,3],9],[[9,0],[0,7]]]]
         |[[[5,[7,4]],7],1]
         |[[[[4,2],2],6],[8,7]]""".stripMargin, "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"),
      ("""[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
         |[[[5,[2,8]],4],[5,[[9,9],0]]]
         |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
         |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
         |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
         |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
         |[[[[5,4],[7,7]],8],[[8,3],8]]
         |[[9,3],[[9,9],[6,[4,9]]]]
         |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
         |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".stripMargin, "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]")

    ).foreach { case (input, expected) =>
      s"$input" in {
        val inputSFN = Day18.parser(input).toOption.get
        val expectedSFN = Day18.parseLine(expected).toOption.get

        assert(Day18.reduceList(inputSFN) === expectedSFN)
      }
    }
  }

  "magnitude" should {
    "work for the example" in {
      val sfn = Day18.parseLine("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]").toOption.get
      assert(Day18.magnitude(sfn) === 4140)
    }
  }
