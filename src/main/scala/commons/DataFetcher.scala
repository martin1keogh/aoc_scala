package commons

import java.io.FileWriter
import java.nio.file.{Files, Path, Paths}
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.Try

case class DataFetcher(puzzle: Puzzle[_, _]):
  type Input = String

  def getInput: Input =
    readLocalFile getOrElse {
      val downloaded = downloadInput
      saveInput(downloaded)
      downloaded
    }

  private val localInputFile: Path =
    Paths.get("data", puzzle.year.toString, puzzle.day.toString)

  // Should probably make sure to close this.
  // If only I cared.
  private def readLocalFile: Option[Input] =
    Try(Source.fromFile(localInputFile.toFile).getLines().mkString).toOption

  private def downloadInput: String =
    requests
      .get(
        puzzle.inputUrl.toString,
        headers = Map("cookie" -> s"session=${sys.env("SESSION")}")
      )
      .text()

  private def saveInput(input: String): Unit =
    ()
//    Files.createDirectories(localInputFile.getParent)
//    Files.write(localInputFile, input.linesIterator.toList.asJava)
