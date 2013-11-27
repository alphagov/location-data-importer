package uk.gov.gds.logging

import uk.gov.gds.io.reportWriter
import org.joda.time.DateTime


sealed abstract class Error(errorType: String)
object RowParseError extends Error("row-parse-error")
object MissingLpiError extends Error("no-active-lpi-for-uprn")
object MissingClassificationError extends Error("no-active-classification-for-uprn")
object FileError extends Error("file-error")


object Reporter {

  reportWriter.writeStrings(List("\n", "=== Starting Run at " + new DateTime + " ===\n"))

  def report(fileName: String, errorType: Error, errorData: Option[String] = None) {
    report(List(fileName, errorType, errorData.getOrElse("")).mkString(",") + "\n")
  }

  def report(lines: List[String])  {
    reportWriter.writeStrings(lines, "\n")
  }

  def report(line: String)  {
    reportWriter.write(line)
  }
}
