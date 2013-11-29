package uk.gov.gds.testutils


import uk.gov.gds.io.loadFile
import uk.gov.gds.logging.Reporter._
import java.io.File
import scala.util.Random
import scala.collection.immutable.List

object ReporterTestUtils {

  def loadReport = loadFile(new File(reportFile))

  def reportLines = loadReport.lines()

  lazy val random = new Random()

  def randomFilename = List.fill(10)(random.nextPrintableChar()).mkString

  def reportLineToTest(fileName: String) = reportLines.filter(_.startsWith(fileName)).headOption
}
