package aoc2021

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.implicits.*
import commons.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

case class Coord(x: Int, y: Int, z: Int):
  def +(other: Coord): Coord =
    Coord(x + other.x, y + other.y, z + other.z)

  type Dir = 1 | -1

  def *(directions: (Dir, Dir, Dir)): Coord =
    Coord(x * directions._1, y * directions._2, z * directions._3)

  def unary_- : Coord =
    Coord(-x, -y, -z)

  def distanceTo(other: Coord): Int =
    math.abs(x - other.x) + math.abs(y - other.y) + math.abs(z - other.z)

object Day19 extends Solver[List[List[Coord]], Int] :
  val puzzle: Puzzle[List[List[Coord]], Int] = Puzzle(Year(2021), Day(19))

  override def parser(input: String): ValidatedNec[String, List[List[Coord]]] =
    input.split("\\R\\R").toList.traverse { groupString =>
      val _ :: beacons = groupString.linesIterator.toList
      beacons.traverse { beacon =>
        beacon.split(",").map(_.toIntOption) match {
          case Array(Some(x), Some(y), Some(z)) => Valid(Coord(x, y, z))
          case err => Invalid(NonEmptyChain(err.mkString))
        }
      }
    }

  override def part1(beacons: List[List[Coord]]): Option[Int] =
    val (result, _) = solve(beacons)
    Some(result)

  override def part2(beacons: List[List[Coord]]): Option[Int] =
    val (_, scanners) = solve(beacons)
    scanners.combinations(2).map {
      case sc1 :: sc2 :: Nil => sc1.distanceTo(sc2)
      case _ => ???
    }.maxOption

  def solve(beacons: List[List[Coord]]): (Int, List[Coord]) =
    val head :: others = beacons
    val toProcess = mutable.Queue.from(others)
    var result = head
    val scanners = ListBuffer.empty[Coord]
    while toProcess.nonEmpty do
      val next = toProcess.head
      toProcess.dropInPlace(1)
      reconciliate(result, next) match
        case Some((scanner, matches)) =>
          result ++= matches
          result = result.distinct
          scanners += scanner
        case None =>
          toProcess.append(next)
    result.length -> scanners.toList

  def reconciliate(b1: List[Coord], b2: List[Coord]): Option[(Coord, List[Coord])] =
    val orientations = b2.orientations
    val choices: Iterator[(Set[Coord], Coord, List[Coord])] =
      for {
        anchorPoint <- b1.iterator
        orientatedCoords <- orientations
        sourcePoint <- orientatedCoords.iterator
        diff = anchorPoint + -sourcePoint
        shifted = orientatedCoords.map(_ + diff)
        matches = b1.toSet intersect shifted.toSet
        if matches.size >= 12
      } yield (matches, diff, shifted)

    choices.maxByOption(_._1.size).map(_.tail)


  extension (coords: List[Coord])
    def orientations: List[List[Coord]] =
      for {
        rotation <- List[Coord => Coord](
          c => Coord(c.x, c.y, c.z),
          c => Coord(c.y, c.x, c.z),
          c => Coord(c.x, c.z, c.y),
          c => Coord(c.z, c.x, c.y),
          c => Coord(c.z, c.y, c.x),
          c => Coord(c.y, c.z, c.x),
        )
        direction <- List[(1 | -1, 1 | -1, 1 | -1)](
          (1, 1, 1),
          (1, 1, -1),
          (1, -1, 1),
          (-1, 1, 1),
          (1, -1, -1),
          (-1, 1, -1),
          (-1, -1, 1),
          (-1, -1, -1),
        )
      } yield coords.map(coord => rotation(coord) * direction).distinct
