package uk.gov.gds.model

import org.specs2.mutable.Specification
import scalax.io.{CloseableIterator, DefaultResourceContext, LineTraversable}
import scalax.io.Line.Terminators.NewLine
import processors._
import scala.collection.mutable
import uk.gov.gds.io._
import uk.gov.gds.testutils.ReporterTestUtils._
import uk.gov.gds.model.CodeLists._
import org.joda.time.DateTime
import java.io.File
import scala.Some

class ExtractorsTests extends Specification {

  private val validLinesForBLPU = List(
    """21,"I",94755,9059007610,1,2,2005-04-05,9059007610,346782.00,732382.00,1,9059,2005-04-05,,2009-05-22,2005-04-05,"S","DD5 3BX",0""",
    """21,"I",94755,9059007611,1,2,2005-04-05,9059007610,346782.00,732382.00,1,9059,2005-04-05,,2009-05-22,2005-04-05,"S","DD5 3BY",0""",
    """21,"I",94755,9059007612,1,2,2005-04-05,9059007610,346782.00,732382.00,1,9059,2005-04-05,,2009-05-22,2005-04-05,"S","DD5 3BZ",0"""
  )

  private val validLinesForLPI = List(
    """24,"I",92423,9059007610,"9059L000069680","ENG",1,2005-04-05,,2005-04-05,2005-04-05,99,"SAO Start Suffix",100,"SAO End Suffix","Sao Text",1,"PAO Start Suffix",2,"PAO End Suffix","PAO Text",7803241,"1","Area 51","level","Y"""",
    """24,"I",92423,9059007611,"9059L000069680","ENG",1,2005-04-05,,2005-04-05,2005-04-05,99,"SAO Start Suffix",100,"SAO End Suffix","Sao Text",1,"PAO Start Suffix",2,"PAO End Suffix","PAO Text",7803242,"1","Area 51","level","Y"""",
    """24,"I",92423,9059007612,"9059L000069680","ENG",1,2005-04-05,,2005-04-05,2005-04-05,99,"SAO Start Suffix",100,"SAO End Suffix","Sao Text",1,"PAO Start Suffix",2,"PAO End Suffix","PAO Text",7803243,"1","Area 51","level","Y""""
  )

  private val validLinesForClassification = List(
    """32,"I",94733,9059007610,"9059C000003726","C","AddressBase Premium Classification Scheme",1.0,2010-04-22,,2012-01-11,2010-04-22""",
    """32,"I",94733,9059007611,"9059C000003726","R","AddressBase Premium Classification Scheme",1.0,2010-04-23,,2012-01-12,2010-04-23""",
    """32,"I",94733,9059007612,"9059C000003726","RD04","AddressBase Premium Classification Scheme",1.0,2010-04-24,,2012-01-13,2010-04-24"""
  )

  private val validLinesForOrganisation = List(
    """31,"I",93718,9059007610,"9059O000000971","Hannahs","",2012-02-01,,2012-02-01,2012-02-01""",
    """31,"I",93835,9059007611,"9059O000001679","Party Time","",2012-03-20,,2012-03-20,2012-03-20""",
    """31,"I",93843,9059007612,"9059O000000619","Department Of Leisure And Culture","",2012-01-23,,2012-01-23,2012-01-23"""
  )

  private val validLinesForStreets = List(
    """11,"I",1,7803241,1,9059,,1995-09-04,,8,0,1995-09-04,,2005-04-07,2005-04-07,345558.00,731129.00,345809.00,731128.00,999""",
    """11,"I",1,7803242,1,9059,,1995-09-04,,8,0,1995-09-04,,2005-04-07,2005-04-07,345558.00,731129.00,345809.00,731128.00,999""",
    """11,"I",1,7803243,1,9059,,1995-09-04,,8,0,1995-09-04,,2005-04-07,2005-04-07,345558.00,731129.00,345809.00,731128.00,999"""
  )

  private val validLinesForStreetDescriptor = List(
    """15,"I",1146,7803241,"ACCESS FROM ZC4 TO GAGIE HOLDING","","MURROES","ANGUS","ENG"""",
    """15,"I",1148,7803242,"ACCESS FROM ZU306 TO B962 AT LAWS","NEWBIGGING","MONIFIETH","ANGUS","ENG"""",
    """15,"I",1150,7803243,"ZU306 FROM TRACK BETWEEN ZU306 AND B962 TO B961 AT DRUMSTURDY","","KINGENNIE","ANGUS","ENG""""
  )

  import extractors._
  import uk.gov.gds.io.parseCsvLine

  "Processers" should {

    "correctly process a file for code point rows" in {
      processRowsIntoCodePoints(new File("testdata/codepoint/good-file.csv"))(None).size must beEqualTo(1)
      val result = processRowsIntoCodePoints(new File("testdata/codepoint/good-file.csv"))(None)

      processRowsIntoCodePoint(new File("testdata/codepoint/good-file.csv"))(0).postcode must beEqualTo("dd97yx")
      processRowsIntoCodePoint(new File("testdata/codepoint/good-file.csv"))(0).country must beEqualTo("S92000003")
      processRowsIntoCodePoint(new File("testdata/codepoint/good-file.csv"))(0).county must beEqualTo(None)
      processRowsIntoCodePoint(new File("testdata/codepoint/good-file.csv"))(0).district must beEqualTo("S12000041")
      processRowsIntoCodePoint(new File("testdata/codepoint/good-file.csv"))(0).ward must beEqualTo("S13002509")
    }

    "correctly process a file for with many code point rows" in {
      processRowsIntoCodePoint(new File("testdata/codepoint/good-file-with-many-rows.csv")).size must beEqualTo(13)
    }

    "correctly process a file for with many code point rows - excluding rows that are invalid" in {
      processRowsIntoCodePoint(new File("testdata/codepoint/file-with-one-bad-row.csv")).size must beEqualTo(12)
    }

    "correctly process a file for streets" in {
      processRowsIntoStreets(new File("testdata/addressbase/single-good-file/good-file.csv")).size must beEqualTo(1)
      processRowsIntoStreets(new File("testdata/addressbase/single-good-file/good-file.csv"))(0).usrn must beEqualTo("7803555")
    }

    "correctly process a file for addresses" in {
      processRowsIntoAddressWrappers(new File("testdata/addressbase/single-good-file/good-file.csv")).size must beEqualTo(1)
      processRowsIntoAddressWrappers(new File("testdata/addressbase/single-good-file/good-file.csv"))(0).blpu.uprn must beEqualTo("9059007610")
      processRowsIntoAddressWrappers(new File("testdata/addressbase/single-good-file/good-file.csv"))(0).lpi.paoStartNumber.get must beEqualTo("2")
      processRowsIntoAddressWrappers(new File("testdata/addressbase/single-good-file/good-file.csv"))(0).classification.classificationCode must beEqualTo("RD")
      processRowsIntoAddressWrappers(new File("testdata/addressbase/single-good-file/good-file.csv"))(0).organisation.get.organistation must beEqualTo("Party Time")

    }
  }

  "Extractors" should {
    "extract a BLPU from a parsed line from the address base file" in {
      val validBlpuLine = """21,"I",94755,9059007610,1,2,2005-04-05,9059007610,346782.00,732382.00,1,9059,2005-04-05,2010-04-05,2009-05-22,2005-04-05,"S","DD5 3BX",0"""
      val blpuOption = extractRow[BLPU]("filename", parseCsvLine(validBlpuLine), BLPU)

      blpuOption must not be (None)
      blpuOption.get.uprn must beEqualTo("9059007610")
    }

    "throw an exception if presented with an invalid BLPU line from the address base file" in {
      val filename = randomFilename
      val blpuLineWithNoUPRN = """21,"I",94755,,1,2,2005-04-05,9059007610,346782.00,732382.00,1,9059,2005-04-05,2010-04-05,2009-05-22,2005-04-05,"S","DD5 3BX",0"""
      extractRow[BLPU](filename, parseCsvLine(blpuLineWithNoUPRN), BLPU) must throwA[Exception]
      reportLineToTest(filename) must not be None
      reportLineToTest(filename).get must contain("row-parse-error")
      reportLineToTest(filename).get must contain(parseCsvLine(blpuLineWithNoUPRN).mkString("|"))
    }

    "extract a LPI from a parsed line from the address base file" in {
      val lpiLine = """24,"I",92423,9059007612,"9059L000069680","ENG",1,2005-04-05,2006-04-01,2005-04-05,2005-04-05,99,"SAO Start Suffix",100,"SAO End Suffix","Sao Text",1,"PAO Start Suffix",2,"PAO End Suffix","PAO Text",7803243,"1","Area 51","level","Y""""
      val lpiOption = extractRow[LPI]("filename", parseCsvLine(lpiLine), LPI)
      lpiOption must not be (None)
      lpiOption.get.uprn must beEqualTo("9059007612")
    }

    "throw an exception if presented with an invalid BLPU line from the address base file" in {
      val filename = randomFilename
      val lpiLineWithNoUPRN = """21,"I",94755,,1,2,2005-04-05,9059007610,346782.00,732382.00,1,9059,2005-04-05,2010-04-05,2009-05-22,2005-04-05,"S","DD5 3BX",0"""
      extractRow[LPI](filename, parseCsvLine(lpiLineWithNoUPRN), LPI) must throwA[Exception]
      reportLineToTest(filename) must not be None
      reportLineToTest(filename).get must contain("row-parse-error")
      reportLineToTest(filename).get must contain(parseCsvLine(lpiLineWithNoUPRN).mkString("|"))
    }

    "extract a Classification from a parsed line from the address base file" in {
      val classificationLine = """32,"I",94733,9059004873,"9059C000003726","RD04","AddressBase Premium Classification Scheme",1.0,2010-04-24,,2012-01-13,2010-04-24"""
      val classificationOption = extractRow[Classification]("filename", parseCsvLine(classificationLine), Classification)
      classificationOption must not be (None)
      classificationOption.get.uprn must beEqualTo("9059004873")
    }

    "throw an exception if presented with an invalid Classification line from the address base file" in {
      val filename = randomFilename

      val lineWithNoUPRN = """32,"I",94733,,"9059C000003726","RD04","AddressBase Premium Classification Scheme",1.0,2010-04-24,,2012-01-13,2010-04-24"""
      extractRow[Classification](filename, parseCsvLine(lineWithNoUPRN), Classification) must throwA[Exception]
      reportLineToTest(filename) must not be None
      reportLineToTest(filename).get must contain("row-parse-error")
      reportLineToTest(filename).get must contain(parseCsvLine(lineWithNoUPRN).mkString("|"))
    }

    "extract a Organisation from a parsed line from the address base file" in {
      val organisationLine = """31,"I",93843,9059000379,"9059O000000619","Department Of Leisure And Culture","",2012-01-23,,2012-01-23,2012-01-23"""
      val organisationOption = extractRow[Organisation]("filename", parseCsvLine(organisationLine), Organisation)
      organisationOption must not be (None)
      organisationOption.get.uprn must beEqualTo("9059000379")
    }

    "throw an exception if presented with an invalid Organisation line from the address base file" in {
      val filename = randomFilename
      val lineWithNoUPRN = """31,"I",93843,,"9059O000000619","Department Of Leisure And Culture","",2012-01-23,,2012-01-23,2012-01-23"""
      extractRow[Organisation](filename, parseCsvLine(lineWithNoUPRN), Organisation) must throwA[Exception]
      reportLineToTest(filename) must not be None
      reportLineToTest(filename).get must contain("row-parse-error")
      reportLineToTest(filename).get must contain(parseCsvLine(lineWithNoUPRN).mkString("|"))
    }

    "extract a Street  from a parsed line from the address base file" in {
      val streetLine = """11,"I",1,7803555,1,9059,,1995-09-04,,8,0,1995-09-04,,2005-04-07,2005-04-07,345558.00,731129.00,345809.00,731128.00,999"""
      val streetOption = extractRow[Street]("filename", parseCsvLine(streetLine), Street)
      streetOption must not be (None)
      streetOption.get.usrn must beEqualTo("7803555")
    }

    "throw an exception if presented with an invalid Street line from the address base file" in {
      val filename = randomFilename
      val streetLineWithNoUSRN = """11,"I",1,,1,9059,,1995-09-04,,8,0,1995-09-04,,2005-04-07,2005-04-07,345558.00,731129.00,345809.00,731128.00,999"""
      extractRow[Street](filename, parseCsvLine(streetLineWithNoUSRN), Street) must throwA[Exception]
      reportLineToTest(filename) must not be None
      reportLineToTest(filename).get must contain("row-parse-error")
      reportLineToTest(filename).get must contain(parseCsvLine(streetLineWithNoUSRN).mkString("|"))
    }

    "extract a Street Descriptor from a parsed line from the address base file" in {
      val streetDescriptorLine = """15,"I",1150,709887,"ZU306 FROM TRACK BETWEEN ZU306 AND B962 TO B961 AT DRUMSTURDY","","KINGENNIE","ANGUS","ENG""""
      val streetDescriptorOption = extractRow[StreetDescriptor]("filename", parseCsvLine(streetDescriptorLine), StreetDescriptor)
      streetDescriptorOption must not be (None)
      streetDescriptorOption.get.usrn must beEqualTo("709887")
    }

    "throw an exception if presented with an invalid Street Descriptor line from the address base file" in {
      val filename = randomFilename
      val lineWithNoUSRN = """15,"I",1150,,"ZU306 FROM TRACK BETWEEN ZU306 AND B962 TO B961 AT DRUMSTURDY","","KINGENNIE","ANGUS","ENG""""
      extractRow[StreetDescriptor](filename, parseCsvLine(lineWithNoUSRN), StreetDescriptor) must throwA[Exception]
      reportLineToTest(filename) must not be None
      reportLineToTest(filename).get must contain("row-parse-error")
      reportLineToTest(filename).get must contain(parseCsvLine(lineWithNoUSRN).mkString("|"))
    }

    "should be able to extract all the BLPUs from a list of AddressBase objects" in {
      extractBlpus(buildListOfAddressBaseObjects).foreach(blpu => blpu.isInstanceOf[BLPU] must beTrue)
      extractBlpus(buildListOfAddressBaseObjects).size must beEqualTo(3)
    }

    "should be able to extract all the Street Descriptors from a list of AddressBase objects" in {
      extractStreetDescriptors(buildListOfAddressBaseObjects).foreach(street => street.isInstanceOf[StreetDescriptor] must beTrue)
      extractStreetDescriptors(buildListOfAddressBaseObjects).size must beEqualTo(3)
    }

    "should be able to extract all the Streets from a list of AddressBase objects, grouped by UPRN" in {
      extractStreetsByUsrn(buildListOfAddressBaseObjects).size must beEqualTo(3)
      extractStreetsByUsrn(buildListOfAddressBaseObjects).get("7803241").get(0).isInstanceOf[Street] must beEqualTo(true)
      extractStreetsByUsrn(buildListOfAddressBaseObjects).get("7803242").get(0).isInstanceOf[Street] must beEqualTo(true)
      extractStreetsByUsrn(buildListOfAddressBaseObjects).get("7803243").get(0).isInstanceOf[Street] must beEqualTo(true)
      extractStreetsByUsrn(buildListOfAddressBaseObjects).get("7803241").size must beEqualTo(1)
      extractStreetsByUsrn(buildListOfAddressBaseObjects).get("7803242").size must beEqualTo(1)
      extractStreetsByUsrn(buildListOfAddressBaseObjects).get("7803243").size must beEqualTo(1)
    }


    "should be able to extract all the LPIs from a list of AddressBase objects, grouped by UPRN" in {
      extractLpisByUprn(buildListOfAddressBaseObjects).size must beEqualTo(3)
      extractLpisByUprn(buildListOfAddressBaseObjects).get("9059007610").get(0).isInstanceOf[LPI] must beEqualTo(true)
      extractLpisByUprn(buildListOfAddressBaseObjects).get("9059007611").get(0).isInstanceOf[LPI] must beEqualTo(true)
      extractLpisByUprn(buildListOfAddressBaseObjects).get("9059007612").get(0).isInstanceOf[LPI] must beEqualTo(true)
      extractLpisByUprn(buildListOfAddressBaseObjects).get("9059007610").size must beEqualTo(1)
      extractLpisByUprn(buildListOfAddressBaseObjects).get("9059007611").size must beEqualTo(1)
      extractLpisByUprn(buildListOfAddressBaseObjects).get("9059007612").size must beEqualTo(1)
    }

    "should be able to extract all the Organisations from a list of AddressBase objects, grouped by UPRN" in {
      extractOrganisationsUprn(buildListOfAddressBaseObjects).size must beEqualTo(3)
      extractOrganisationsUprn(buildListOfAddressBaseObjects).get("9059007610").get(0).isInstanceOf[Organisation] must beEqualTo(true)
      extractOrganisationsUprn(buildListOfAddressBaseObjects).get("9059007611").get(0).isInstanceOf[Organisation] must beEqualTo(true)
      extractOrganisationsUprn(buildListOfAddressBaseObjects).get("9059007612").get(0).isInstanceOf[Organisation] must beEqualTo(true)
      extractOrganisationsUprn(buildListOfAddressBaseObjects).get("9059007610").size must beEqualTo(1)
      extractOrganisationsUprn(buildListOfAddressBaseObjects).get("9059007611").size must beEqualTo(1)
      extractOrganisationsUprn(buildListOfAddressBaseObjects).get("9059007612").size must beEqualTo(1)
    }

    "should be able to extract all the Classifications from a list of AddressBase objects, grouped by UPRN" in {
      extractClassificationsByUprn(buildListOfAddressBaseObjects).size must beEqualTo(3)
      extractClassificationsByUprn(buildListOfAddressBaseObjects).get("9059007610").get(0).isInstanceOf[Classification] must beEqualTo(true)
      extractClassificationsByUprn(buildListOfAddressBaseObjects).get("9059007611").get(0).isInstanceOf[Classification] must beEqualTo(true)
      extractClassificationsByUprn(buildListOfAddressBaseObjects).get("9059007612").get(0).isInstanceOf[Classification] must beEqualTo(true)
      extractClassificationsByUprn(buildListOfAddressBaseObjects).get("9059007610").size must beEqualTo(1)
      extractClassificationsByUprn(buildListOfAddressBaseObjects).get("9059007611").size must beEqualTo(1)
      extractClassificationsByUprn(buildListOfAddressBaseObjects).get("9059007612").size must beEqualTo(1)
    }

    "should be get most recently updated LPI from a list of LPIs" in {
      val lpi1 = lpi.copy(lastUpdated = new DateTime().minusDays(1))
      val lpi2 = lpi.copy(lastUpdated = new DateTime().minusDays(2))

      mostRecentLPIForUprn("uprn", Map("uprn" -> List(lpi1, lpi2))).get must beEqualTo(lpi1)
    }

    "should be get most recently updated LPI from a list of LPIs excluding those with an end date" in {
      val lpi1 = lpi.copy(lastUpdated = new DateTime().minusDays(1), endDate = Some(new DateTime))
      val lpi2 = lpi.copy(lastUpdated = new DateTime().minusDays(2))
      val lpi3 = lpi.copy(lastUpdated = new DateTime().minusDays(3))

      mostRecentLPIForUprn("uprn", Map("uprn" -> List(lpi1, lpi2, lpi3))).get must beEqualTo(lpi2)
    }

    "should be returning none if no valid LPI available" in {
      val lpi1 = lpi.copy(lastUpdated = new DateTime().minusDays(1), endDate = Some(new DateTime))

      mostRecentLPIForUprn("uprn", Map("uprn" -> List(lpi1))) must beEqualTo(None)
    }

    "should be get most recently updated Classification from a list of Classifications" in {
      val classification1 = classification.copy(lastUpdated = new DateTime().minusDays(1))
      val classification2 = classification.copy(lastUpdated = new DateTime().minusDays(2))

      mostRecentClassificationForUprn("uprn", Map("uprn" -> List(classification1, classification2))).get must beEqualTo(classification1)
    }

    "should be get most recently updated Classification from a list of Classifications excluding those with an end date" in {
      val classification1 = classification.copy(lastUpdated = new DateTime().minusDays(1), endDate = Some(new DateTime))
      val classification2 = classification.copy(lastUpdated = new DateTime().minusDays(2))
      val classification3 = classification.copy(lastUpdated = new DateTime().minusDays(3))

      mostRecentClassificationForUprn("uprn", Map("uprn" -> List(classification1, classification2, classification3))).get must beEqualTo(classification2)
    }

    "should be returning none if no valid Classification available" in {
      val classification1 = classification.copy(lastUpdated = new DateTime().minusDays(1), endDate = Some(new DateTime))

      mostRecentClassificationForUprn("uprn", Map("uprn" -> List(classification1))) must beEqualTo(None)
    }

    "should be get most recently updated Organisation from a list of Organisations" in {
      val organisation1 = organisation.copy(lastUpdated = new DateTime().minusDays(1))
      val organisation2 = organisation.copy(lastUpdated = new DateTime().minusDays(2))

      mostRecentOrganisationForUprn("uprn", Map("uprn" -> List(organisation1, organisation2))).get must beEqualTo(organisation1)
    }

    "should be get most recently updated Organisation from a list of Organisations excluding those with an end date" in {
      val organisation1 = organisation.copy(lastUpdated = new DateTime().minusDays(1), endDate = Some(new DateTime))
      val organisation2 = organisation.copy(lastUpdated = new DateTime().minusDays(2))
      val organisation3 = organisation.copy(lastUpdated = new DateTime().minusDays(3))

      mostRecentOrganisationForUprn("uprn", Map("uprn" -> List(organisation1, organisation2, organisation3))).get must beEqualTo(organisation2)
    }

    "should be returning none if no valid Organisation available" in {
      val organisation1 = organisation.copy(lastUpdated = new DateTime().minusDays(1), endDate = Some(new DateTime))

      mostRecentOrganisationForUprn("uprn", Map("uprn" -> List(organisation1))) must beEqualTo(None)
    }

    "should be get most recently updated Street from a list of street" in {
      val street1 = street.copy(lastUpdated = new DateTime().minusDays(1))
      val street2 = street.copy(lastUpdated = new DateTime().minusDays(2))

      mostRecentStreetForUsrn("usrn", Map("usrn" -> List(street1, street2))).get must beEqualTo(street1)
    }

    "should be get most recently updated Street from a list of Street excluding those with an end date" in {
      val street1 = street.copy(lastUpdated = new DateTime().minusDays(1), endDate = Some(new DateTime))
      val street2 = street.copy(lastUpdated = new DateTime().minusDays(2))
      val street3 = street.copy(lastUpdated = new DateTime().minusDays(3))

      mostRecentStreetForUsrn("usrn", Map("usrn" -> List(street1, street2, street3))).get must beEqualTo(street2)
    }

    "should be returning none if no valid Street available" in {
      val street1 = street.copy(lastUpdated = new DateTime().minusDays(1), endDate = Some(new DateTime))

      mostRecentStreetForUsrn("usrn", Map("usrn" -> List(street1))) must beEqualTo(None)
    }

    "should build an address wrapper linking the blpu to the correct LPI and classification" in {
      val lpi1 = lpi.copy(lastUpdated = new DateTime().minusDays(1), endDate = Some(new DateTime))
      val lpi2 = lpi.copy(lastUpdated = new DateTime().minusDays(2))
      val lpi3 = lpi.copy(lastUpdated = new DateTime().minusDays(3))
      val classification1 = classification.copy(lastUpdated = new DateTime().minusDays(1), endDate = Some(new DateTime))
      val classification2 = classification.copy(lastUpdated = new DateTime().minusDays(2))
      val classification3 = classification.copy(lastUpdated = new DateTime().minusDays(3))

      val addressWrapper = buildAddressWrapper(randomFilename, validBlpu, Map("uprn" -> List(lpi1, lpi2, lpi3)), Map("uprn" -> List(classification1, classification2, classification3)), Map.empty[String, List[Organisation]])
      addressWrapper must not be (None)
      addressWrapper.get.blpu must beEqualTo(validBlpu)
      addressWrapper.get.lpi must beEqualTo(lpi2)
      addressWrapper.get.classification must beEqualTo(classification2)
    }

    "should not build an address wrapper, returning None if no valid LPI" in {
      val filename = randomFilename
      val classification1 = classification.copy(lastUpdated = new DateTime().minusDays(3))

      val addressWrapper = buildAddressWrapper(filename, validBlpu, Map.empty, Map("uprn" -> List(classification1)), Map.empty[String, List[Organisation]])
      addressWrapper must be(None)
      reportLineToTest(filename) must not be None
      reportLineToTest(filename).get must contain("no-lpi-for-uprn")
      reportLineToTest(filename).get must contain("uprn")
    }

    "should not build an address wrapper, returning None if no active valid LPI" in {
      val filename = randomFilename
      val lpi1 = lpi.copy(lastUpdated = new DateTime().minusDays(1), endDate = Some(new DateTime))
      val classification1 = classification.copy(lastUpdated = new DateTime().minusDays(3))

      val addressWrapper = buildAddressWrapper(filename, validBlpu, Map("uprn" -> List(lpi1)), Map("uprn" -> List(classification1)), Map.empty[String, List[Organisation]])
      addressWrapper must be(None)
      reportLineToTest(filename) must not be None
      reportLineToTest(filename).get must contain("no-active-lpi-for-uprn")
      reportLineToTest(filename).get must contain("uprn")
    }

    "should not build an address wrapper, returning None if no valid Classification" in {
      val filename = randomFilename
      val classification1 = classification.copy(endDate = Some(new DateTime().minusDays(3)))

      val addressWrapper = buildAddressWrapper(filename, validBlpu, Map("uprn" -> List(lpi)), Map("uprn" -> List(classification1)), Map.empty[String, List[Organisation]])
      addressWrapper must be(None)
      reportLineToTest(filename) must not be None
      reportLineToTest(filename).get must contain("no-active-classification-for-uprn")
      reportLineToTest(filename).get must contain("uprn")
    }

    "should build an street wrapper linking the street descriptor to the correct street" in {
      val street1 = street.copy(lastUpdated = new DateTime().minusDays(1), endDate = Some(new DateTime))
      val street2 = street.copy(lastUpdated = new DateTime().minusDays(2))
      val street3 = street.copy(lastUpdated = new DateTime().minusDays(3))
      val filename = randomFilename

      val streetWrapper = buildStreetWrapper(filename, Map("usrn" -> List(street1, street2, street3)), streetDescriptor)
      streetWrapper must not be (None)
      streetWrapper.get.usrn must beEqualTo("usrn")
      streetWrapper.get.localityName must beEqualTo(streetDescriptor.localityName)
      streetWrapper.get.administrativeArea must beEqualTo(streetDescriptor.administrativeArea)
      streetWrapper.get.townName must beEqualTo(streetDescriptor.townName)
      streetWrapper.get.streetDescription must beEqualTo(streetDescriptor.streetDescription)
      streetWrapper.get.state must beEqualTo(Some("open"))
      streetWrapper.get.surface must beEqualTo(Some("mixed"))
      streetWrapper.get.recordType must beEqualTo(Some("numberedStreet"))
      streetWrapper.get.classification must beEqualTo(Some("allVehicles"))
      streetWrapper.get.file must beEqualTo(filename)
    }

    "should not build an street wrapper, returning None if no valid Street" in {
      val filename = randomFilename
      val street1 = street.copy(lastUpdated = new DateTime().minusDays(1), endDate = Some(new DateTime))

      val streetWrapper = buildStreetWrapper(filename, Map("usrn" -> List(street1)), streetDescriptor)
      streetWrapper must be(None)
      reportLineToTest(filename) must not be None
      reportLineToTest(filename).get must contain("no-active-street-for-usrn")
      reportLineToTest(filename).get must contain("usrn")
    }

    "should not build an street wrapper, returning None if no Street for usrn" in {
      val filename = randomFilename

      val streetWrapper = buildStreetWrapper(filename, Map.empty, streetDescriptor)
      streetWrapper must be(None)
      reportLineToTest(filename) must not be None
      reportLineToTest(filename).get must contain("no-street-for-usrn")
      reportLineToTest(filename).get must contain("usrn")
    }

    "should extract street wrappers from list of address base objects" in {
      val streetWrappers = extractStreetWrappers(randomFilename, buildListOfAddressBaseObjects)
      streetWrappers.size must beEqualTo(3)
    }

    "should extract streets with description from list of address base objects" in {

      val streetsWithOneInvalidLine = List(
        """11,"I",1,7803241,1,9059,,1995-09-04,,8,0,1995-09-04,,2005-04-07,2005-04-07,345558.00,731129.00,345809.00,731128.00,999""",
        """11,"I",1,7803242,1,9059,,1995-09-04,,8,0,1995-09-04,2006-01-01,2005-04-07,2005-04-07,345558.00,731129.00,345809.00,731128.00,999""",
        """11,"I",1,7803243,1,9059,,1995-09-04,,8,0,1995-09-04,,2005-04-07,2005-04-07,345558.00,731129.00,345809.00,731128.00,999"""
      )

      val streetDescriptors = validLinesForStreetDescriptor.flatMap(line => extractRow[StreetDescriptor]("filename", parseCsvLine(line), StreetDescriptor))
      val streets = streetsWithOneInvalidLine.flatMap(line => extractRow[Street]("filename", parseCsvLine(line), Street))

      val streetWithDescriptions = extractStreetWrappers(randomFilename, List(streets, streetDescriptors).flatten)
      streetWithDescriptions.size must beEqualTo(2)
      streetWithDescriptions.filter(a => a.usrn.equalsIgnoreCase("7803241")).size must beEqualTo(1)
      streetWithDescriptions.filter(a => a.usrn.equalsIgnoreCase("7803242")).size must beEqualTo(0)
      streetWithDescriptions.filter(a => a.usrn.equalsIgnoreCase("7803243")).size must beEqualTo(1)
    }

    "should extract address base wrappers from list of address base objects" in {
      val addressWrappers = extractAddressBaseWrappers(randomFilename, buildListOfAddressBaseObjects)
      addressWrappers.size must beEqualTo(3)
    }

    "should extract address base wrappers from list of address base objects - excluding those with an end date" in {

      val blpus = validLinesForBLPU.flatMap(line => extractRow[BLPU]("filename", parseCsvLine(line), BLPU)).map(blpu => blpu.copy(endDate = Some(new DateTime)))
      val lpis = validLinesForLPI.flatMap(line => extractRow[LPI]("filename", parseCsvLine(line), LPI))
      val organisations = validLinesForOrganisation.flatMap(line => extractRow[Organisation]("filename", parseCsvLine(line), Organisation))
      val classifications = validLinesForClassification.flatMap(line => extractRow[Classification]("filename", parseCsvLine(line), Classification))

      val addressWrappers = extractAddressBaseWrappers(randomFilename, List(blpus, lpis, organisations, classifications).flatten)
      addressWrappers.size must beEqualTo(0)
    }

    "should extract address base wrappers from list of address base objects" in {

      val lpiLinesWithOneInvalidLine = List(
        """24,"I",92423,9059007610,"9059L000069680","ENG",1,2005-04-05,,2005-04-05,2005-04-05,99,"SAO Start Suffix",100,"SAO End Suffix","Sao Text",1,"PAO Start Suffix",2,"PAO End Suffix","PAO Text",7803241,"1","Area 51","level","Y"""",
        """24,"I",92423,9059007611,"9059L000069680","ENG",1,2005-04-05,2010-01-01,2005-04-05,2005-04-05,99,"SAO Start Suffix",100,"SAO End Suffix","Sao Text",1,"PAO Start Suffix",2,"PAO End Suffix","PAO Text",7803242,"1","Area 51","level","Y"""",
        """24,"I",92423,9059007612,"9059L000069680","ENG",1,2005-04-05,,2005-04-05,2005-04-05,99,"SAO Start Suffix",100,"SAO End Suffix","Sao Text",1,"PAO Start Suffix",2,"PAO End Suffix","PAO Text",7803243,"1","Area 51","level","Y""""
      )

      val blpus = validLinesForBLPU.flatMap(line => extractRow[BLPU]("filename", parseCsvLine(line), BLPU))
      val lpis = lpiLinesWithOneInvalidLine.flatMap(line => extractRow[LPI]("filename", parseCsvLine(line), LPI))
      val organisations = validLinesForOrganisation.flatMap(line => extractRow[Organisation]("filename", parseCsvLine(line), Organisation))
      val classifications = validLinesForClassification.flatMap(line => extractRow[Classification]("filename", parseCsvLine(line), Classification))

      val addressWrappers = extractAddressBaseWrappers(randomFilename, List(blpus, lpis, organisations, classifications).flatten)
      addressWrappers.size must beEqualTo(2)
      addressWrappers.filter(a => a.blpu.uprn.equalsIgnoreCase("9059007610")).size must beEqualTo(1)
      addressWrappers.filter(a => a.blpu.uprn.equalsIgnoreCase("9059007611")).size must beEqualTo(0)
      addressWrappers.filter(a => a.blpu.uprn.equalsIgnoreCase("9059007612")).size must beEqualTo(1)
    }
  }

  def buildListOfAddressBaseObjects = {
    val streetsDescriptions = validLinesForStreetDescriptor.flatMap(line => extractRow[StreetDescriptor]("filename", parseCsvLine(line), StreetDescriptor))
    val streets = validLinesForStreets.flatMap(line => extractRow[Street]("filename", parseCsvLine(line), Street))
    val blpus = validLinesForBLPU.flatMap(line => extractRow[BLPU]("filename", parseCsvLine(line), BLPU))
    val lpis = validLinesForLPI.flatMap(line => extractRow[LPI]("filename", parseCsvLine(line), LPI))
    val organisations = validLinesForOrganisation.flatMap(line => extractRow[Organisation]("filename", parseCsvLine(line), Organisation))
    val classifications = validLinesForClassification.flatMap(line => extractRow[Classification]("filename", parseCsvLine(line), Classification))
    List(streets, streetsDescriptions, blpus, lpis, organisations, classifications).flatten
  }

  private lazy val startDate = new DateTime().minusDays(100)
  private lazy val lastUpdatedDate = new DateTime().minusDays(50)

  private lazy val validBlpu = BLPU(
    "uprn",
    Some(BlpuStateCode.inUse),
    Some(LogicalStatusCode.approved),
    1.1,
    2.2,
    "1234",
    startDate,
    None,
    lastUpdatedDate,
    "S",
    "postcode")

  private lazy val lpi = LPI(
    "uprn",
    "usrn",
    Some(LogicalStatusCode.approved),
    startDate,
    None,
    lastUpdatedDate,
    Some("pao start number"),
    Some("pao start suffix"),
    Some("pao end number"),
    Some("pao end suffix"),
    Some("pao text"),
    Some("sao start number"),
    Some("sao start suffix"),
    Some("sao end number"),
    Some("sao end suffix"),
    Some("sao text"),
    Some("area name"),
    Some(true)
  )


  private lazy val street = Street("usrn", Some(StreetRecordTypeCode.numberedStreet), Some(StreetStateCode.open), Some(StreetSurfaceCode.mixed), Some(StreetClassificationCode.allVehicles), startDate, None, lastUpdatedDate)
  private lazy val streetDescriptor = StreetDescriptor("usrn", "description", Some("locality"), Some("town"), "admin area")
  private lazy val classification = Classification("uprn", "code", startDate, None, lastUpdatedDate)
  private lazy val organisation = Organisation("uprn", "organisation", startDate, None, lastUpdatedDate)

}
