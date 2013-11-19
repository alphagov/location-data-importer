package uk.gov.gds.io

import uk.gov.gds.logging.Logging

import uk.gov.gds.model.Transformers.processRows
import scala.collection._
import uk.gov.gds.model.BLPU


object ProcessAddressBaseFiles extends Logging {

  def process(filePath: String): Result = {
    filePathHasErrors(filePath) match {
      case Some(error) => error
      case _ => processFiles(filePath)
    }
  }

  private def processFiles(filePath: String) = {
    val errors = mutable.MutableList.empty[String]
    val rows = directoryContents(filePath).flatMap(f => processRows(loadFile(f).lines())(errors, f.getName))
    if (errors.isEmpty) {
      Result(Success, "Processed [" + rows.filter(row => row.isInstanceOf[BLPU]).size + "] addressable objects")
    } else {
      Result(Some(Failure), errors.toList)
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
