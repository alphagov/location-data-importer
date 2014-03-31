package uk.gov.gds.location.importer.io

import java.io.File
import com.Ostermiller.util.CSVParser
import scalax.io.{Resource, Codec}

object FileUtilities {

  implicit val code: Codec = Codec("UTF-8")

  /* wrappers for outcome of parse attempt */

  sealed abstract class Outcome(r: Boolean)

  case object Success extends Outcome(true)

  case object Failure extends Outcome(false)

  case class Result(outcome: Outcome, message: String)

  /* File utility methods */

  def loadFile(file: File) = Resource.fromFile(file)

  def writer(location: String) = Resource.fromFile(location).writer

  def fileExists(location: String) = new File(location).exists

  def deleteFile(file: File) = file.delete()

  def isDirectory(location: String) = new File(location).isDirectory

  def directoryContents(location: String) = new File(location).listFiles().toList

  def filteredDirectoryContents(location: String, fileFilter: File => Boolean) = directoryContents(location).filter(fileFilter)

  def parseCsvLine(line: String) = CSVParser.parse(line)(0).toList
}

