package uk.gov.gds.location.importer

import uk.gov.gds.location.importer.logging.Logging
import uk.gov.gds.location.importer.io.FileUtilities._
import scala.Some
import uk.gov.gds.location.importer.processors.AddressBaseFileProcessors

/**
 * Class to wrap the file aspects of the process
 * (1) Checks directories
 * (2) Passes file list to processor class
 * (3) Collates results into a simple how many True and how many False? count
 * (4) For each type return a wrapper object to feed back this collated outcome count
 * @param processors AddressBaseFileProcessors
 */
class ProcessAddressBaseFiles(processors: AddressBaseFileProcessors) extends Logging {

  def processCodePointFiles(filePath: String): Result =
    filePathHasErrors(filePath) match {
      case Some(error) => error
      case _ => processForCodePoints(filePath)
    }

  def processAddressBaseFilesForStreets(filePath: String): Result =
    filePathHasErrors(filePath) match {
      case Some(error) => error
      case _ => processForStreets(filePath)
    }

  def processAddressBaseFilesForAddresses(filePath: String): Result =
    filePathHasErrors(filePath) match {
      case Some(error) => error
      case _ => processForAddresses(filePath)
    }


  private def processForCodePoints(filePath: String) = resultOf("codepoint", directoryContents(filePath).map(processors.processCodePointFile(_)).toList)

  private def processForStreets(filePath: String) = resultOf("streets", directoryContents(filePath).map(processors.processAddressBaseForStreets(_)).toList)

  private def processForAddresses(filePath: String) = resultOf("addresses", directoryContents(filePath).map(processors.processAddressBaseForAddresses(_)).toList)

  private def resultOf(pass: String, fileResult: List[Boolean]) = {

    /*
      Results partitioned on result type, 1) Success 2) Failure
     */
    val overallResult = fileResult.partition(result => true)

    /*
      Partitions used to count success / error rows
     */
    overallResult match {
      case results if results._2.isEmpty => Result(Success, "processed " + pass + ": [" + overallResult._1.size + "] files")
      case _ => Result(Failure, "processed " + pass + ": " + overallResult._1.size + " files successfully and " + overallResult._2.size + " files with errors")
    }
  }
  private def filePathHasErrors(filePath: String): Option[Result] = {
    if (!fileExists(filePath)) Some(Result(Failure, "Supplied path does not exist"))
    else if (!isDirectory(filePath)) Some(Result(Failure, "Supplied path is not a directory"))
    else if (directoryContents(filePath).isEmpty) Some(Result(Failure, filePath + " contains no files"))
    else if (allFilesAreNotCsvFiles(filePath)) Some(Result(Failure, filePath + " contains files that are not csv files [" + nonCsvFilesFor(filePath).mkString(", ") + "]"))
    else None
  }

  private def allFilesAreNotCsvFiles(filePath: String) = !nonCsvFilesFor(filePath).isEmpty

  private def nonCsvFilesFor(filePath: String) = directoryContents(filePath).filter(file => !file.getName.endsWith(".csv"))
}
