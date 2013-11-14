package uk.gov.gds

import java.io.File
import com.Ostermiller.util.CSVParser
import scalax.io.{Resource, Codec}

package object io {

  sealed abstract class Outcome(r: Boolean)

  case object Success extends Outcome(true)

  case object Failure extends Outcome(false)

  case class Result(outcome: Outcome, message: String)

  implicit val code: Codec = Codec("UTF-8")

  def loadFile(fileName: String) = Resource.fromFile(fileName)

  def fileExists(location: String) = new File(location).exists

  def isDirectory(location: String) = new File(location).isDirectory

  def directoryContents(location: String) = new File(location).listFiles().toList

  def filteredDirectoryContents(location: String, fileFilter: File => Boolean) = directoryContents(location).filter(fileFilter)

  def parseCsvLine(line: String) = CSVParser.parse(line)(0).toList
}
