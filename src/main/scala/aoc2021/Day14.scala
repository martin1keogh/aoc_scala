package aoc2021

import cats.Traverse
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.implicits.*
import commons.*

import scala.annotation.tailrec
import scala.util.Try

object Day14 extends Solver[(String, Map[String, String]), Long] :
  val puzzle = Puzzle(Year(2021), Day(14))

  override def parser(input: String): ValidatedNec[String, (String, Map[String, String])] =
    val Array(template, mappingList) = input.split("\\R\\R")
    val mappings = mappingList.linesIterator.toList.map { line =>
      line.split(" -> ") match
        case Array(source, target) if source.length == 2 && target.length == 1 => Valid(source -> target)
        case _ => Invalid(NonEmptyChain("Invalid mapping"))
    }.sequence

    mappings.map(template -> _.toMap)

  override def part1(input: (String, Map[String, String])): Option[Long] =
    val (template, mappings) = input
    val polymer10 =
      Range.inclusive(1, 10).foldLeft(template) { case (polymer, _) =>
        polymer.toList.sliding(2).map {
          case c1 :: c2 :: Nil => c1 + mappings(c1 + c2.toString)
        }.mkString + polymer.last
      }

    val frequencies = polymer10.groupMapReduce(identity)(_ => 1L)(_ + _)
    Some(frequencies.maxBy(_._2)._2 - frequencies.minBy(_._2)._2)

  override def part2(input: (String, Map[String, String])): Option[Long] =
    val (template, mappings) = input
    val init = template.sliding(2).toList.groupMapReduce(identity)(_ => 1L)(_ + _)

    val polymer40 =
      Range.inclusive(1, 40).foldLeft(init) { case (counts, _) =>
        val newKeys = counts.toList.map { case (k, count) => (k(0) + mappings(k)) -> count }.groupMapReduce(_._1)(_._2)(_ + _)
        val newKeysB = counts.toList.map { case (k, count) => (mappings(k) + k(1)) -> count }.groupMapReduce(_._1)(_._2)(_ + _)
        newKeys |+| newKeysB
      }

    val frequencies = polymer40
      .toList
      .flatMap { case (k, count) => List(k(0) -> count, k(1) -> count) }
      .groupMapReduce(_._1)(_._2)(_ + _)
      .view.mapValues(_ / 2)
      .toMap

    // the division by 2 above remove 1 too many from both boundaries
    val fixedFreq =
      frequencies
        .updated(template.head, frequencies(template.head) + 1)
        .updated(template.last, frequencies(template.last) + 1)

    Some(fixedFreq.maxBy(_._2)._2 - fixedFreq.minBy(_._2)._2)
