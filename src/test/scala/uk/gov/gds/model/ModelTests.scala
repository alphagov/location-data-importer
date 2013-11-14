package uk.gov.gds.model

import org.specs2.mutable.Specification

import uk.gov.gds.io.parseCsvLine
import org.joda.time.DateTime
import uk.gov.gds.model.CodeLists.BlpuStateCode._
import uk.gov.gds.model.CodeLists.LogicalStatusCode._

class ModelTests extends Specification {


  "BLPU" should {
    "be able to be constructed from a fully populated csv line" in {
      val line = """21,"I",94755,9059007610,1,2,2005-04-05,9059007610,346782.00,732382.00,1,9059,2005-04-05,2010-04-05,2009-05-22,2005-04-05,"S","DD5 3BZ",0"""
      blpu(line).uprn must beEqualTo("9059007610")
      blpu(line).blpuState.get must beEqualTo(inUse)
      blpu(line).logicalState.get must beEqualTo(approved)
      blpu(line).startDate must beEqualTo(new DateTime(2005, 4, 5, 0, 0))
      blpu(line).lastUpdated must beEqualTo(new DateTime(2009, 5, 22, 0, 0))
      blpu(line).endDate.get must beEqualTo(new DateTime(2010, 4, 5, 0, 0))
      blpu(line).xCoordinate must beEqualTo(346782.00)
      blpu(line).yCoordinate must beEqualTo(732382.00)
      blpu(line).postcode must beEqualTo("DD5 3BZ")
    }
  }

  private def blpu(line: String) = BLPU.fromLineOfCsv(parseCsvLine(line))
}
