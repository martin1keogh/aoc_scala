package commons

import java.net.URL
import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.int.{<=, >=}

opaque type Year = Int & Singleton
opaque type Day = Int & Singleton

object Year:
  def apply[T <: Int & Singleton](year: T)(using T >= 2020 =:= true): Year = year

object Day:
  def apply[T <: Int & Singleton](day: T)(using (T >= 1 && T <= 25) =:= true): Day = day

case class Puzzle[I, O](year: Year, day: Day):
  private[commons] val inputUrl: URL = new URL(s"https://adventofcode.com/$year/day/$day/input")
  private[commons] val resourceFile: URL = new URL(s"https://adventofcode.com/$year/day/$day/input")
