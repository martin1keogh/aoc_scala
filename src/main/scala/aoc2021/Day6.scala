package aoc2021

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.implicits.*
import commons.*

import scala.annotation.tailrec
import scala.util.Try

object Day6 extends Solver[List[Int], Long] :
  val puzzle = Puzzle(Year(2021), Day(6))

  def parser(input: String): ValidatedNec[String, List[Int]] =
    input.split(',').toList.traverse { s => s.toIntOption.toValid(NonEmptyChain.of("Not an int")) }

  override def part1(fishes: List[Int]): Option[Long] =
    val count: Long = simulateFishEvolution(fishes, 80)
    Some(count)

  override def part2(fishes: List[Int]): Option[Long] =
    import collection.mutable.{Map => MMap}

    val ages: MMap[Int, Long] = MMap.from(fishes.groupBy(identity).view.mapValues(_.length.toLong))
    val count = (1 to 256).foldLeft(ages) { case (ages, _) =>
      // it's 11pm, don't care anymore
      val gen0 = ages.remove(0).getOrElse(0L)
      ages.update(0, ages.getOrElse(1, 0))
      ages.update(1, ages.getOrElse(2, 0))
      ages.update(2, ages.getOrElse(3, 0))
      ages.update(3, ages.getOrElse(4, 0))
      ages.update(4, ages.getOrElse(5, 0))
      ages.update(5, ages.getOrElse(6, 0))
      ages.update(6, ages.getOrElse(7, 0L) + gen0)
      ages.update(7, ages.getOrElse(8, 0))
      ages.update(8, gen0)
      ages
    }.values.sum
    Some(count)

  def step(fish: Int): Set[Int] =
    if (fish == 0) Set(6, 8)
    else Set(fish - 1)

  @tailrec
  def simulateFishEvolution(fishes: List[Int], nbDays: Int, currDay: Int = 0): Long =
    if (currDay == nbDays) fishes.length
    else simulateFishEvolution(fishes.flatMap(step), nbDays, currDay + 1)
