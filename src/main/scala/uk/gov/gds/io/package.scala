package uk.gov.gds

import java.io.File
import com.Ostermiller.util.CSVParser
import scalax.io.{Resource, Codec}

package object io {

  implicit val code: Codec = Codec("UTF-8")

  /* wrappers for outcome of parse attempt */

  sealed abstract class Outcome(r: Boolean)

  case object Success extends Outcome(true)

  case object Failure extends Outcome(false)

  case class Result(outcome: Outcome, messages: List[String] = List.empty[String])

  object Result {
    def apply(outcome: Outcome, message: String):Result = Result(outcome, List(message))
  }


  /* File utility methods */

  def loadFile(file: File) = Resource.fromFile(file)

  def fileExists(location: String) = new File(location).exists

  def isDirectory(location: String) = new File(location).isDirectory

  def directoryContents(location: String) = new File(location).listFiles().toList

  def filteredDirectoryContents(location: String, fileFilter: File => Boolean) = directoryContents(location).filter(fileFilter)

  def parseCsvLine(line: String) = CSVParser.parse(line)(0).toList
}
