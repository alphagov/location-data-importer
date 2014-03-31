package uk.gov.gds.testutils


import uk.gov.gds.location.importer.io.loadFile
import uk.gov.gds.location.importer.logging.Reporter._
import java.io.File
import scala.util.Random
import scala.collection.immutable.List

object ReporterTestUtils {

  def loadReport = loadFile(new File(reportFile))

  def loadProcessed = loadFile(new File(processed))

  def reportLines = loadReport.lines()

  def processedLines = loadProcessed.lines()

  lazy val random = new Random()

  def randomFilename = List.fill(10)(random.nextPrintableChar()).mkString

  def reportLineToTest(fileName: String) = reportLines.filter(_.startsWith(fileName)).headOption

  def processedLineToTest(fileName: String) = processedLines.filter(_.contains(fileName)).headOption
}

