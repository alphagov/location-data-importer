package uk.gov.gds.model

import org.specs2.mutable.Specification

import uk.gov.gds.io.parseCsvLine
import org.joda.time.DateTime
import uk.gov.gds.model.CodeLists.BlpuStateCode._
import uk.gov.gds.model.CodeLists.LogicalStatusCode._
import uk.gov.gds.model.CodeLists.StreetClassificationCode._
import uk.gov.gds.model.CodeLists.StreetRecordTypeCode._
import uk.gov.gds.model.CodeLists.StreetStateCode._
import uk.gov.gds.model.CodeLists.StreetSurfaceCode._

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

    "be able to be constructed from a minimum populated csv line" in {
      val validLine = """24,"I",92423,9059082524,"9059L000069680","ENG",1,2005-04-05,,2005-04-05,2005-04-05,,"",,"","",,"",,"","",7803241,"1","","","""""
      lpi(validLine).uprn must beEqualTo("9059082524")
      lpi(validLine).usrn must beEqualTo("7803241")
      lpi(validLine).logicalState.get must beEqualTo(approved)
      lpi(validLine).startDate must beEqualTo(new DateTime(2005, 4, 5, 0, 0))
      lpi(validLine).endDate must beEqualTo(None)
      lpi(validLine).lastUpdated must beEqualTo(new DateTime(2005, 4, 5, 0, 0))
      lpi(validLine).paoStartNumber must beEqualTo(None)
      lpi(validLine).paoStartSuffix must beEqualTo(None)
      lpi(validLine).paoEndNumber must beEqualTo(None)
      lpi(validLine).paoEndSuffix must beEqualTo(None)
      lpi(validLine).paoText must beEqualTo(None)
      lpi(validLine).saoStartNumber must beEqualTo(None)
      lpi(validLine).saoStartSuffix must beEqualTo(None)
      lpi(validLine).saoEndNumber must beEqualTo(None)
      lpi(validLine).saoEndSuffix must beEqualTo(None)
      lpi(validLine).saoText must beEqualTo(None)
      lpi(validLine).areaName must beEqualTo(None)
      lpi(validLine).officialAddress must beEqualTo(None)

    }

    "be able to be made into correct type" in {
      val validLine = """24,"I",92423,9059082524,"9059L000069680","ENG",1,2005-04-05,2006-04-01,2005-04-05,2005-04-05,99,"SAO Start Suffix",100,"SAO End Suffix","Sao Text",1,"PAO Start Suffix",2,"PAO End Suffix","PAO Text",7803241,"1","Area 51","level","Y""""
      lpi(validLine).isInstanceOf[AddressBase] must beTrue
      lpi(validLine).isInstanceOf[LPI] must beTrue
    }
  }

  "Street" should {

    val completeValidLine = """11,"I",1151,709895,2,9053,2,2010-02-05,1,8,0,2008-01-25,2010-01-01,2008-10-09,2008-01-25,347600.00,734728.00,347561.00,734677.00,999"""
    val incompleteValidLine = """11,"I",1151,709895,2,9053,,,,,0,2008-01-25,,2008-10-09,2008-01-25,347600.00,734728.00,347561.00,734677.00,999"""

    "be able to identify a valid line" in {
      Street.isValidCsvLine(parseCsvLine(completeValidLine)) must beTrue
    }

    "be able to identify an invalid line due to wrong type" in {
      val wrongRecordIdentifier = """17,"I",1151,709895,2,9053,2,2010-02-05,1,8,0,2008-01-25,2010-01-01,2008-10-09,2008-01-25,347600.00,734728.00,347561.00,734677.00,999"""
      Street.isValidCsvLine(parseCsvLine(wrongRecordIdentifier)) must beFalse
    }

    "be able to identify an invalid line due to wrong number of columns" in {
      val wrongNumberOfColumns = """17,"I",709895,2,9053,,,,,0,2008-01-25,,2008-10-09,2008-01-25,347600.00,734728.00,347561.00,734677.00,999"""
      Street.isValidCsvLine(parseCsvLine(wrongNumberOfColumns)) must beFalse
    }

    "be able to be constructed from a fully populated csv line" in {
      street(completeValidLine).usrn must beEqualTo("709895")
      street(completeValidLine).recordType.get must beEqualTo(streetDescription)
      street(completeValidLine).state.get must beEqualTo(open)
      street(completeValidLine).surface.get must beEqualTo(metalled)
      street(completeValidLine).classification.get must beEqualTo(allVehicles)
      street(completeValidLine).startDate must beEqualTo(new DateTime(2008, 1, 25, 0, 0))
      street(completeValidLine).endDate.get must beEqualTo(new DateTime(2010, 1, 1, 0, 0))
      street(completeValidLine).lastUpdated must beEqualTo(new DateTime(2008, 10, 9, 0, 0))
    }

    "be able to be constructed from a minimum populated csv line" in {
      street(incompleteValidLine).usrn must beEqualTo("709895")
      street(incompleteValidLine).recordType.get must beEqualTo(streetDescription)
      street(incompleteValidLine).state must beEqualTo(None)
      street(incompleteValidLine).surface must beEqualTo(None)
      street(incompleteValidLine).classification must beEqualTo(None)
      street(incompleteValidLine).startDate must beEqualTo(new DateTime(2008, 1, 25, 0, 0))
      street(incompleteValidLine).endDate must beEqualTo(None)
      street(incompleteValidLine).lastUpdated must beEqualTo(new DateTime(2008, 10, 9, 0, 0))

    }

    "be able to be made into correct type" in {
      street(completeValidLine).isInstanceOf[AddressBase] must beTrue
      street(completeValidLine).isInstanceOf[Street] must beTrue
    }
  }


//  "LPI" should {
//
//    "be able to identify a valid line" in {
//      val validLine = """24,"I",92423,9059082524,"9059L000069680","ENG",1,2005-04-05,,2005-04-05,2005-04-05,,"",,"","UNIT 1",,"",,"","WEST PITKERRO",7803241,"1","","","Y""""
//      LPI.isValidCsvLine(parseCsvLine(validLine)) must beTrue
//    }
//
//    "be able to identify an invalid line due to wrong type" in {
//      val wrongRecordIdentifier = """1,"I",92423,9059082524,"9059L000069680","ENG",1,2005-04-05,,2005-04-05,2005-04-05,,"",,"","UNIT 1",,"",,"","WEST PITKERRO",7803241,"1","","","Y""""
//      LPI.isValidCsvLine(parseCsvLine(wrongRecordIdentifier)) must beFalse
//    }
//
//    "be able to identify an invalid line due to wrong number of columns" in {
//      val wrongNumberOfColumns = """24,92423,9059082524,"9059L000069680","ENG",1,2005-04-05,,2005-04-05,2005-04-05,,"",,"","UNIT 1",,"",,"","WEST PITKERRO",7803241,"1","","","Y""""
//      LPI.isValidCsvLine(parseCsvLine(wrongNumberOfColumns)) must beFalse
//    }
//
//    "be able to be constructed from a fully populated csv line" in {
//      val validLine = """24,"I",92423,9059082524,"9059L000069680","ENG",1,2005-04-05,2006-04-01,2005-04-05,2005-04-05,99,"SAO Start Suffix",100,"SAO End Suffix","Sao Text",1,"PAO Start Suffix",2,"PAO End Suffix","PAO Text",7803241,"1","Area 51","level","Y""""
//      lpi(validLine).uprn must beEqualTo("9059082524")
//      lpi(validLine).usrn must beEqualTo("7803241")
//      lpi(validLine).logicalState.get must beEqualTo(approved)
//      lpi(validLine).startDate must beEqualTo(new DateTime(2005, 4, 5, 0, 0))
//      lpi(validLine).endDate.get must beEqualTo(new DateTime(2006, 4, 1, 0, 0))
//      lpi(validLine).lastUpdated must beEqualTo(new DateTime(2005, 4, 5, 0, 0))
//      lpi(validLine).paoStartNumber.get must beEqualTo("1")
//      lpi(validLine).paoStartSuffix.get must beEqualTo("PAO Start Suffix")
//      lpi(validLine).paoEndNumber.get must beEqualTo("2")
//      lpi(validLine).paoEndSuffix.get must beEqualTo("PAO End Suffix")
//      lpi(validLine).paoText.get must beEqualTo("PAO Text")
//      lpi(validLine).saoStartNumber.get must beEqualTo("99")
//      lpi(validLine).saoStartSuffix.get must beEqualTo("SAO Start Suffix")
//      lpi(validLine).saoEndNumber.get must beEqualTo("100")
//      lpi(validLine).saoEndSuffix.get must beEqualTo("SAO End Suffix")
//      lpi(validLine).saoText.get must beEqualTo("Sao Text")
//      lpi(validLine).areaName.get must beEqualTo("Area 51")
//      lpi(validLine).officialAddress.get must beTrue
//
//    }
//
//    "be able to be constructed from a minimum populated csv line" in {
//      val validLine = """24,"I",92423,9059082524,"9059L000069680","ENG",1,2005-04-05,,2005-04-05,2005-04-05,,"",,"","",,"",,"","",7803241,"1","","","""""
//      lpi(validLine).uprn must beEqualTo("9059082524")
//      lpi(validLine).usrn must beEqualTo("7803241")
//      lpi(validLine).logicalState.get must beEqualTo(approved)
//      lpi(validLine).startDate must beEqualTo(new DateTime(2005, 4, 5, 0, 0))
//      lpi(validLine).endDate must beEqualTo(None)
//      lpi(validLine).lastUpdated must beEqualTo(new DateTime(2005, 4, 5, 0, 0))
//      lpi(validLine).paoStartNumber must beEqualTo(None)
//      lpi(validLine).paoStartSuffix must beEqualTo(None)
//      lpi(validLine).paoEndNumber must beEqualTo(None)
//      lpi(validLine).paoEndSuffix must beEqualTo(None)
//      lpi(validLine).paoText must beEqualTo(None)
//      lpi(validLine).saoStartNumber must beEqualTo(None)
//      lpi(validLine).saoStartSuffix must beEqualTo(None)
//      lpi(validLine).saoEndNumber must beEqualTo(None)
//      lpi(validLine).saoEndSuffix must beEqualTo(None)
//      lpi(validLine).saoText must beEqualTo(None)
//      lpi(validLine).areaName must beEqualTo(None)
//      lpi(validLine).officialAddress must beEqualTo(None)
//
//    }
//
//    "be able to be made into correct type" in {
//      val validLine = """24,"I",92423,9059082524,"9059L000069680","ENG",1,2005-04-05,2006-04-01,2005-04-05,2005-04-05,99,"SAO Start Suffix",100,"SAO End Suffix","Sao Text",1,"PAO Start Suffix",2,"PAO End Suffix","PAO Text",7803241,"1","Area 51","level","Y""""
//      lpi(validLine).isInstanceOf[AddressBase] must beTrue
//      lpi(validLine).isInstanceOf[LPI] must beTrue
//    }
//  }


  private def blpu(line: String) = BLPU.fromCsvLine(parseCsvLine(line))

  private def lpi(line: String) = LPI.fromCsvLine(parseCsvLine(line))

  private def street(line: String) = Street.fromCsvLine(parseCsvLine(line))

  private def streetDescriptor(line: String) = StreetDescriptor.fromCsvLine(parseCsvLine(line))
}
