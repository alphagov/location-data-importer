package uk.gov

import uk.gov.gds.io.loadFile
import uk.gov.gds.logging.Reporter._
import java.io.File

object ReporterTestUtils {

  def loadReport = loadFile(new File(reportFile))

  def reportLines = loadReport.lines()
}
