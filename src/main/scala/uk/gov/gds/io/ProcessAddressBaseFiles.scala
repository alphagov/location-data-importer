package uk.gov.gds.io

import uk.gov.gds.model.BLPU
import uk.gov.gds.logging.Logging
import scalax.io.LongTraversable

object ProcessAddressBaseFiles extends Logging {

  def process(filePath: String): Result = {
    filePathHasErrors(filePath) match {
      case Some(error) => error
      case _ => processFiles(filePath)
    }
  }

  private def processFiles(filePath: String) = {

    def process(lines: LongTraversable[String]) = lines.flatMap(line => {
      if (line.startsWith(BLPU.recordIdentifier)) Some(BLPU.fromCsvLine(parseCsvLine(line)))
      else None
    }).toList

    val blpus = directoryContents(filePath).flatMap(f => process(loadFile(f).lines()))

    Result(Success, "Processed [" + blpus.size + "] BLPUs")
  }

  private def filePathHasErrors(filePath: String) = {
    if (!fileExists(filePath)) Some(Result(Failure, "Supplied path does not exist"))
    else if (!isDirectory(filePath)) Some(Result(Failure, "Supplied path is not a directory"))
    else if (directoryContents(filePath).isEmpty) Some(Result(Failure, filePath + " contains no files"))
    else None
  }


}
