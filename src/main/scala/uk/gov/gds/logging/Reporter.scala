package uk.gov.gds.logging

import uk.gov.gds.io.writer
import org.joda.time.DateTime


sealed abstract class Error(errorType: String) {
  override def toString = errorType
}

object InvalidBlpuError extends Error("invalid-blpu")

object NoStreetForBlpuError extends Error("no-street-for-blpu")

object NoCodePointForPostcode extends Error("no-code-point-for-postcode")

object RowParseError extends Error("row-parse-error")

object MissingLpiError extends Error("no-lpi-for-uprn")

object MissingActiveLpiError extends Error("no-active-lpi-for-uprn")

object MissingActiveStreetError extends Error("no-active-street-for-usrn")

object MissingStreetError extends Error("no-street-for-usrn")

object MissingClassificationError extends Error("no-active-classification-for-uprn")

object FileError extends Error("file-error")


object Reporter {

  val reportFile = "/tmp/location-data-import-report.txt"
  val processed = "/tmp/location-data-file-list.txt"

  writer(reportFile).writeStrings(List("\n", "=== Starting Run at " + new DateTime + " ===\n"))

  def report(fileName: String, errorType: Error, errorData: List[String]) {
    report((List(fileName, errorType) ++ errorData).mkString(",") + "\n")
  }


  def report(fileName: String, errorType: Error, errorData: Option[String] = None) {
    report(List(fileName, errorType, errorData.getOrElse("")).mkString(",") + "\n")
  }

  def report(lines: List[String]) {
    synchronized {
      writer(reportFile).writeStrings(lines, "\n")
    }
  }

  def processedFile(pass: String, fileName: String, timeTaken: String) {
    synchronized {
      writer(processed).writeStrings(List(pass, fileName, timeTaken), ",")
      writer(processed).write("\n")
    }
  }

  def report(line: String) {
    synchronized {
      writer(reportFile).write(line)
    }
  }
}
