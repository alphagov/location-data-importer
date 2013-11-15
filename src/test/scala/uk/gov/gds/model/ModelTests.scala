package uk.gov.gds.model

import org.specs2.mutable.Specification

import uk.gov.gds.io.parseCsvLine
import org.joda.time.DateTime
import uk.gov.gds.model.CodeLists.BlpuStateCode._
import uk.gov.gds.model.CodeLists.LogicalStatusCode._

class ModelTests extends Specification {

  "BLPU" should {

    "be able to identify a valid line" in {
      val validLine = """21,"I",94755,9059007610,1,2,2005-04-05,9059007610,346782.00,732382.00,1,9059,2005-04-05,2010-04-05,2009-05-22,2005-04-05,"S","DD5 3BZ",0"""
      BLPU.isValidCsvLine(parseCsvLine(validLine)) must beTrue
    }

    "be able to identify an invalid line due to wrong type" in {
      val wrongRecordIdentifier = """22,"I",94755,9059007610,1,2,2005-04-05,9059007610,346782.00,732382.00,1,9059,2005-04-05,2010-04-05,2009-05-22,2005-04-05,"S","DD5 3BZ",0"""
      BLPU.isValidCsvLine(parseCsvLine(wrongRecordIdentifier)) must beFalse
    }

    "be able to identify an invalid line due to wrong number of columns" in {
      val wrongNumberOfColumns = """21,94755,9059007610,1,2,2005-04-05,9059007610,346782.00,732382.00,1,9059,2005-04-05,2010-04-05,2009-05-22,2005-04-05,"S","DD5 3BZ",0"""
      BLPU.isValidCsvLine(parseCsvLine(wrongNumberOfColumns)) must beFalse
    }

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

    "be able to be constructed from a csv line with only mandatory fields" in {
      val line = """21,"I",94755,9059007610,1,,,,346782.00,732382.00,1,9059,2005-04-05,,2009-05-22,2005-04-05,"S","DD5 3BZ",0"""
      blpu(line).uprn must beEqualTo("9059007610")
      blpu(line).blpuState must beEqualTo(None)
      blpu(line).logicalState.get must beEqualTo(approved)
      blpu(line).startDate must beEqualTo(new DateTime(2005, 4, 5, 0, 0))
      blpu(line).lastUpdated must beEqualTo(new DateTime(2009, 5, 22, 0, 0))
      blpu(line).endDate must beEqualTo(None)
      blpu(line).xCoordinate must beEqualTo(346782.00)
      blpu(line).yCoordinate must beEqualTo(732382.00)
      blpu(line).postcode must beEqualTo("DD5 3BZ")
    }

    "be able to be made into correct type" in {
      val line = """21,"I",94755,9059007610,1,,,,346782.00,732382.00,1,9059,2005-04-05,,2009-05-22,2005-04-05,"S","DD5 3BZ",0"""
      blpu(line).isInstanceOf[AddressBase] must beTrue
      blpu(line).isInstanceOf[BLPU] must beTrue
    }
  }

  "LPI" should {

    "be able to identify a valid line" in {
      val validLine = """24,"I",92423,9059082524,"9059L000069680","ENG",1,2005-04-05,,2005-04-05,2005-04-05,,"",,"","UNIT 1",,"",,"","WEST PITKERRO",7803241,"1","","","Y""""
      LPI.isValidCsvLine(parseCsvLine(validLine)) must beTrue
    }

    "be able to identify an invalid line due to wrong type" in {
      val wrongRecordIdentifier = """1,"I",92423,9059082524,"9059L000069680","ENG",1,2005-04-05,,2005-04-05,2005-04-05,,"",,"","UNIT 1",,"",,"","WEST PITKERRO",7803241,"1","","","Y""""
      LPI.isValidCsvLine(parseCsvLine(wrongRecordIdentifier)) must beFalse
    }

    "be able to identify an invalid line due to wrong number of columns" in {
      val wrongNumberOfColumns = """24,92423,9059082524,"9059L000069680","ENG",1,2005-04-05,,2005-04-05,2005-04-05,,"",,"","UNIT 1",,"",,"","WEST PITKERRO",7803241,"1","","","Y""""
      LPI.isValidCsvLine(parseCsvLine(wrongNumberOfColumns)) must beFalse
    }

    "be able to be constructed from a fully populated csv line" in {
      val validLine = """24,"I",92423,9059082524,"9059L000069680","ENG",1,2005-04-05,2006-04-01,2005-04-05,2005-04-05,99,"SAO Start Suffix",100,"SAO End Suffix","Sao Text",1,"PAO Start Suffix",2,"PAO End Suffix","PAO Text",7803241,"1","Area 51","level","Y""""
      lpi(validLine).uprn must beEqualTo("9059082524")
      lpi(validLine).usrn must beEqualTo("7803241")
      lpi(validLine).logicalState.get must beEqualTo(approved)
      lpi(validLine).startDate must beEqualTo(new DateTime(2005, 4, 5, 0, 0))
      lpi(validLine).endDate.get must beEqualTo(new DateTime(2006, 4, 1, 0, 0))
      lpi(validLine).lastUpdated must beEqualTo(new DateTime(2005, 4, 5, 0, 0))
      lpi(validLine).paoStartNumber.get must beEqualTo("1")
      lpi(validLine).paoStartSuffix.get must beEqualTo("PAO Start Suffix")
      lpi(validLine).paoEndNumber.get must beEqualTo("2")
      lpi(validLine).paoEndSuffix.get must beEqualTo("PAO End Suffix")
      lpi(validLine).paoText.get must beEqualTo("PAO Text")
      lpi(validLine).saoStartNumber.get must beEqualTo("99")
      lpi(validLine).saoStartSuffix.get must beEqualTo("SAO Start Suffix")
      lpi(validLine).saoEndNumber.get must beEqualTo("100")
      lpi(validLine).saoEndSuffix.get must beEqualTo("SAO End Suffix")
      lpi(validLine).saoText.get must beEqualTo("Sao Text")
      lpi(validLine).areaName.get must beEqualTo("Area 51")
      lpi(validLine).officialAddress.get must beTrue

    }
  }

  private def blpu(line: String) = BLPU.fromCsvLine(parseCsvLine(line))

  private def lpi(line: String) = LPI.fromCsvLine(parseCsvLine(line))
}
