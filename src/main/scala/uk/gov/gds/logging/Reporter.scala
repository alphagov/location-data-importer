package uk.gov.gds.logging

import uk.gov.gds.io.writer
import org.joda.time.DateTime


sealed abstract class Error(errorType: String) {
  override def toString = errorType
}

object InvalidBlpuError extends Error("invalid-blpu")

object NoStreetForBlpuError extends Error("no-street-for-blpu")

object RowParseError extends Error("row-parse-error")

object MissingLpiError extends Error("no-active-lpi-for-uprn")

object MissingClassificationError extends Error("no-active-classification-for-uprn")

object FileError extends Error("file-error")


object Reporter {

  val reportFile = "/tmp/location-data-import-report.txt"

  writer(reportFile).writeStrings(List("\n", "=== Starting Run at " + new DateTime + " ===\n"))

  def report(fileName: String, errorType: Error, errorData: Option[String] = None) {
    report(List(fileName, errorType, errorData.getOrElse("")).mkString(",") + "\n")
  }

  def report(lines: List[String]) {
    synchronized {
      writer(reportFile).writeStrings(lines, "\n")
    }
  }

  def report(line: String) {
    synchronized {
      writer(reportFile).write(line)
    }
  }
}
