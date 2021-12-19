package aoc2021

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, NonEmptyMap, Validated, ValidatedNec}
import cats.implicits.*
import commons.*

import scala.annotation.tailrec
import scala.util.Try
import scala.util.parsing.combinator.*


enum SFN:
  case Leaf(v: Int)
  case Node(left: SFN, right: SFN)


object Day18 extends LineBasedInput[SFN], Solver[List[SFN], Int] :

  import SFN.*

  val puzzle = Puzzle(Year(2021), Day(18))

  private class SFNParser extends RegexParsers {
    def number: Parser[Int] = """(\d+)""".r ^^ (_.toInt)

    def leaf: Parser[Leaf] = number ^^ Leaf.apply

    def node: Parser[Node] = "[" ~> sfn ~ "," ~ sfn <~ "]" ^^ {
      case left ~ _ ~ right => Node(left, right)
    }

    def sfn: Parser[SFN] = leaf | node
  }

  private val parser = new SFNParser

  override def parseLine(line: String): Validated[String, SFN] =
    parser.parse(parser.sfn, line) match {
      case parser.Failure(msg, _) => Invalid(msg)
      case parser.Error(msg, _) => Invalid(msg)
      case parser.Success(result, _) => Valid(result)
    }

  override def part1(values: List[SFN]): Option[Int] =
    Some(magnitude(reduceList(values)))

  def reduceList(values: List[SFN]): SFN =
    values.reduceLeft {
      case (sfn1, sfn2) =>
        reduceNode(Node(sfn1, sfn2))
    }

  @tailrec
  def reduceNode(node: SFN): SFN =
    explode(node) orElse split(node) match {
      case Some(newNode) => reduceNode(newNode)
      case None => node
    }

  def explode(sfn: SFN): Option[SFN] =
    def rec(sfn: SFN, depth: Int): (SFN, Option[Int], Option[Int]) = sfn match {
      case l@Leaf(_) =>
        (l, None, None)

      case Node(Leaf(l), Leaf(r)) if depth > 3 =>
        (Leaf(0), Some(l), Some(r))

      case Node(left, right) =>
        val (newLeft, ll, rl) = rec(left, depth + 1)
        if (left != newLeft) {
          (Node(newLeft, addRight(right, rl)), ll, None)
        }
        else {
          val (newRight, lr, rr) = rec(right, depth + 1)
          if (right != newRight) {
            (Node(addLeft(left, lr), newRight), None, rr)
          }
          else {
            (Node(left, right), None, None)
          }
        }
    }

    val newNode = rec(sfn, 0)._1
    if (sfn != newNode) Some(newNode)
    else None

  def addRight(sfn: SFN, v: Option[Int]): SFN = sfn match {
    case Leaf(v2) => Leaf(v.getOrElse(0) + v2)
    case Node(l, r) => Node(addRight(l, v), r)
  }

  def addLeft(sfn: SFN, v: Option[Int]): SFN = sfn match {
    case Leaf(v2) => Leaf(v.getOrElse(0) + v2)
    case Node(l, r) => Node(l, addLeft(r, v))
  }

  def split(sfn: SFN): Option[SFN] =
    def rec(sfn: SFN): SFN = sfn match {
      case Leaf(v) if v >= 10 => Node(Leaf(v / 2), Leaf(math.ceil(v / 2.0).toInt))
      case Leaf(v) => Leaf(v)
      case Node(l, r) =>
        val newLeft = rec(l)
        if (l != newLeft) {
          Node(newLeft, r)
        }
        else {
          val newRight = rec(r)
          if (r != newRight) {
            Node(l, newRight)
          }
          else Node(l, r)
        }
    }
    val newNode = rec(sfn)
    if (sfn != newNode) Some(newNode)
    else None

  def magnitude(sfn: SFN): Int = sfn match {
    case Leaf(v) => v
    case Node(l, r) => 3 * magnitude(l) + 2 * magnitude(r)
  }
