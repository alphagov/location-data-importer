package uk.gov.gds.io

import uk.gov.gds.logging.Logging

import uk.gov.gds.model.Transformers.processRows

object ProcessAddressBaseFiles extends Logging {

  def process(filePath: String): Result = {
    filePathHasErrors(filePath) match {
      case Some(error) => error
      case _ => processFiles(filePath)
    }
  }

  private def processFiles(filePath: String) = {
    val rows = directoryContents(filePath).par.flatMap(f => processRows(loadFile(f).lines()))
    Result(Success, "Processed [" + rows.size + "] rows")
  }

  private def filePathHasErrors(filePath: String) = {
    if (!fileExists(filePath)) Some(Result(Failure, "Supplied path does not exist"))
    else if (!isDirectory(filePath)) Some(Result(Failure, "Supplied path is not a directory"))
    else if (directoryContents(filePath).isEmpty) Some(Result(Failure, filePath + " contains no files"))
    else None
  }


}
