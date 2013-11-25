package uk.gov.gds.io

import uk.gov.gds.logging.Logging

import uk.gov.gds.model.Transformers._
import uk.gov.gds.MongoConnection
import java.io.File


object ProcessAddressBaseFiles extends Logging {

  def processSingleFile(filePath: String)(implicit mongoConnection: Option[MongoConnection]): Result = {
    if (!fileExists(filePath)) Result(Failure, "Supplied path does not exist")
    else processFile(new File(filePath)).get
  }

  def process(filePath: String)(implicit mongoConnection: Option[MongoConnection]): Result = {
    filePathHasErrors(filePath) match {
      case Some(error) => error
      case _ => processFiles(filePath)
    }
  }

  private def processFiles(filePath: String)(implicit mongoConnection: Option[MongoConnection]) = {
    val results = directoryContents(filePath).flatMap(processFile(_))

    if (!results.filter(r => r.outcome.equals(Failure)).isEmpty) {
      val errors = results.filter(r => r.outcome.equals(Failure)).map(failure => failure.messages).flatten
      val success = results.filter(r => r.outcome.equals(Success)).size

      if (success > 0) Result(Failure, errors.::("processed=[" + success + "] files"))
      else Result(Failure, errors)
    } else {
      Result(Success, "processed=[" + results.size + "] files")
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
