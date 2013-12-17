package uk.gov.gds.io

import uk.gov.gds.logging.Logging
import uk.gov.gds.model.processors._
import uk.gov.gds.MongoConnection

object ProcessAddressBaseFiles extends Logging {

  def streets(filePath: String)(implicit mongoConnection: Option[MongoConnection]): Result =
    filePathHasErrors(filePath) match {
      case Some(error) => error
      case _ => processForStreets(filePath)
    }

  def addresses(filePath: String)(implicit mongoConnection: Option[MongoConnection]): Result =
    filePathHasErrors(filePath) match {
      case Some(error) => error
      case _ => processForAddresses(filePath)
    }

  def codePoints(filePath: String)(implicit mongoConnection: Option[MongoConnection]): Result =
    filePathHasErrors(filePath) match {
      case Some(error) => error
      case _ => processForCodePoints(filePath)
    }

  private def processForCodePoints(filePath: String)(implicit mongoConnection: Option[MongoConnection]) = resultOf("codepoint", directoryContents(filePath).par.flatMap(processRowsIntoCodePoints(_)).toList)

  private def processForStreets(filePath: String)(implicit mongoConnection: Option[MongoConnection]) = resultOf("streets", directoryContents(filePath).par.flatMap(processStreets(_)).toList)

  private def processForAddresses(filePath: String)(implicit mongoConnection: Option[MongoConnection]) = resultOf("addresses", directoryContents(filePath).par.flatMap(processAddresses(_)).toList)

  private def resultOf(pass: String, fileResult: List[Result]) = {

    /*
      Results partitioned on result type, 1) Success 2) Failure
     */
    val overallResult = fileResult.partition(result => result.outcome.equals(Success))

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
