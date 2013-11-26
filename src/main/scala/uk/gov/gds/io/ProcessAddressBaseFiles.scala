package uk.gov.gds.io

import uk.gov.gds.logging.{FileError, Logging}
import uk.gov.gds.logging.Reporter._
import uk.gov.gds.model.transformers._
import uk.gov.gds.MongoConnection

object ProcessAddressBaseFiles extends Logging {

  def streets(filePath: String)(implicit mongoConnection: Option[MongoConnection]): Result = {
    filePathHasErrors(filePath) match {
      case Some(error) => error
      case _ => processForStreets(filePath)
    }
  }

  def addresses(filePath: String)(implicit mongoConnection: Option[MongoConnection]): Result = {
    filePathHasErrors(filePath) match {
      case Some(error) => error
      case _ => processForAddresses(filePath)
    }
  }

  private def processForStreets(filePath: String)(implicit mongoConnection: Option[MongoConnection]) = result(directoryContents(filePath).flatMap(processStreets(_)))

  private def processForAddresses(filePath: String)(implicit mongoConnection: Option[MongoConnection]) = result(directoryContents(filePath).flatMap(processAddresses(_)))

  private def result(results: List[Result]) = {
    val r = results.partition(result => result.outcome.equals(Success))

    r match {
      case r._2.isEmpty => Result(Success, "processed " + r._1.size + " files")
      case _ => {
        r._2 foreach (failure => report(failure.messages.head, FileError))
        Result(Failure, "processed " + r._1.size + " files successfully and " + r._2.size + " files with errors")
      }
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
