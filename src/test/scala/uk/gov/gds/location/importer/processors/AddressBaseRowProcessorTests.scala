package uk.gov.gds.location.importer.processors

import org.specs2.mutable.Specification
import scalax.io.LineTraversable
import AddressBaseRowProcessor._
import scalax.io.{CloseableIterator, DefaultResourceContext, LineTraversable}
import scalax.io.Line.Terminators.NewLine
import uk.gov.gds.location.importer.model.CodeLists._
import org.joda.time.DateTime
import scala.Some
import uk.gov.gds.location.importer.model._

class AddressBaseRowProcessorTests extends Specification {

  implicit private def listToLineTraversable(list: List[String]) = new LineTraversable(CloseableIterator(list.mkString("\n").toCharArray.iterator), NewLine, false, DefaultResourceContext)

  /**
   * Process CSV rows into AddressBase objects
   */

  private val validCodePointLines = List[String](
    """"KT9 2QL",10,"N",9,9,3,6,0,9,0,517267,161318,"E92000001","E19000003","E18000007","","E09000021","E05000405","S"""",
    """"KT9 2QN",10,"N",41,41,41,0,0,41,0,517442,164126,"E92000001","E19000003","E18000007","","E09000021","E05000404","S"""",
    """"KT9 2QP",10,"N",9,9,9,0,0,9,0,517413,164223,"E92000001","E19000003","E18000007","","E09000021","E05000404","S""""
  )

  private val invalidCodePointLine = List( """"",10,"N",9,9,9,0,0,9,0,517413,164223,"E92000001","E19000003","E18000007","","E09000021","E05000404","S"""")

  "processRowsIntoCodePoint" should {
    "be able to constuct list of CodePoint objects from a list of valid CSV string" in {
      processRowsIntoCodePoint(validCodePointLines, "fileName").size must beEqualTo(3)
      processRowsIntoCodePoint(validCodePointLines, "fileName")(0).postcode must beEqualTo("kt92ql")
      processRowsIntoCodePoint(validCodePointLines, "fileName")(1).postcode must beEqualTo("kt92qn")
      processRowsIntoCodePoint(validCodePointLines, "fileName")(2).postcode must beEqualTo("kt92qp")
    }

    "be able to constuct list of CodePoint objects from a list of valid CSV string - skipping invalid lines" in {
      val lines = validCodePointLines ++ invalidCodePointLine
      processRowsIntoCodePoint(lines, "fileName").size must beEqualTo(3)
      processRowsIntoCodePoint(lines, "fileName")(0).postcode must beEqualTo("kt92ql")
      processRowsIntoCodePoint(lines, "fileName")(1).postcode must beEqualTo("kt92qn")
      processRowsIntoCodePoint(lines, "fileName")(2).postcode must beEqualTo("kt92qp")
    }

  }

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

  private val inValidLinesForStreets = List( """11,"I",1,,1,9059,,1995-09-04,,8,0,1995-09-04,,2005-04-07,2005-04-07,345558.00,731129.00,345809.00,731128.00,999""")
  private val inValidLinesForStreetDescriptor = List( """15,"I",1150,,"ZU306 FROM TRACK BETWEEN ZU306 AND B962 TO B961 AT DRUMSTURDY","","KINGENNIE","ANGUS","ENG"""")


  "processRowsIntoStreets" should {
    "extract street wrappers from valid lines" in {
      processRowsIntoStreets(validLinesForStreets ++ validLinesForStreetDescriptor, "filename").size must beEqualTo(3)
      processRowsIntoStreets(validLinesForStreets ++ validLinesForStreetDescriptor, "filename")(0).usrn must beEqualTo("7803241")
      processRowsIntoStreets(validLinesForStreets ++ validLinesForStreetDescriptor, "filename")(1).usrn must beEqualTo("7803242")
      processRowsIntoStreets(validLinesForStreets ++ validLinesForStreetDescriptor, "filename")(2).usrn must beEqualTo("7803243")
    }

    "extract street wrappers from invalid lines - should throw exception" in {
      val lines = validLinesForStreets ++ validLinesForStreetDescriptor ++ inValidLinesForStreetDescriptor ++ inValidLinesForStreets
      processRowsIntoStreets(lines, "filename") must throwA[Exception]
    }
  }

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

  private val invaldBlpu = List( """"21","I",94755,,1,2,2005-04-05,9059007610,346782.00,732382.00,1,9059,2005-04-05,,2009-05-22,2005-04-05,"S","DD5 3BZ",0""")

  "processRowsIntoAddressWrappers" should {
    "extract an AddressWrapper for each valid set of consituent address rows" in {
      val lines = validLinesForBLPU ++ validLinesForLPI ++ validLinesForClassification ++ validLinesForOrganisation
      processRowsIntoAddressWrappers(lines, "filename").size must beEqualTo(3)
      processRowsIntoAddressWrappers(lines, "filename")(0).uprn must beEqualTo("9059007610")
      processRowsIntoAddressWrappers(lines, "filename")(1).uprn must beEqualTo("9059007611")
      processRowsIntoAddressWrappers(lines, "filename")(2).uprn must beEqualTo("9059007612")
    }

    "extract AddressWrappers from invalid lines - should throw exception" in {
      val lines = validLinesForBLPU ++ validLinesForLPI ++ validLinesForClassification ++ validLinesForOrganisation ++ invaldBlpu
      processRowsIntoAddressWrappers(lines, "filename") must throwA[Exception]
    }
  }

  /**
   * Extract specific AddressBase type from list of generic types
   */

  "extractBlpus" should {
    "extract all BLPU types from a list of generic AddressBase types" in {
      val blpuToFind = blpu("uprn")
      extractBlpus(List(blpuToFind, lpi("uprn", "usrn"), classification("uprn"), organisation("uprn"))).size must beEqualTo(1)
      extractBlpus(List(blpuToFind, lpi("uprn", "usrn"), classification("uprn"), organisation("uprn")))(0) must beEqualTo(blpuToFind)
    }
  }

  "extractLpisByUprn" should {
    "extract all LPI types from a list of generic AddressBase types grouping by UPRN" in {
      val lpi1 = lpi("uprn", "usrn")
      val lpi2 = lpi("uprn", "usrn")
      extractLpisByUprn(List(blpu("uprn"), lpi1, lpi2, classification("uprn"), organisation("uprn")))("uprn").size must beEqualTo(2)
      extractLpisByUprn(List(blpu("uprn"), lpi1, lpi2, classification("uprn"), organisation("uprn")))("uprn") must contain(lpi1, lpi2)
    }
  }

  "extractOrganisationsUprn" should {
    "extract all Organisation types from a list of generic AddressBase types grouping by UPRN" in {
      val org1 = organisation("uprn")
      val org2 = organisation("uprn")
      extractOrganisationsUprn(List(blpu("uprn"), lpi("uprn", "usrn"), classification("uprn"), org1, org2))("uprn").size must beEqualTo(2)
      extractOrganisationsUprn(List(blpu("uprn"), lpi("uprn", "usrn"), classification("uprn"), org1, org2))("uprn") must contain(org1, org2)
    }
  }

  "extractClassificationsByUprn" should {
    "extract all Classification types from a list of generic AddressBase types grouping by UPRN" in {
      val class1 = classification("uprn")
      val class2 = classification("uprn")
      extractClassificationsByUprn(List(blpu("uprn"), lpi("uprn", "usrn"), class1, class2, organisation("uprn")))("uprn").size must beEqualTo(2)
      extractClassificationsByUprn(List(blpu("uprn"), lpi("uprn", "usrn"), class1, class2, organisation("uprn")))("uprn") must contain(class1, class2)
    }
  }

  "extractStreetDescriptors" should {
    "extract all StreetDescriptors types from a list of generic AddressBase types" in {
      val streetDescriptor1 = streetDescriptor("usrn")
      extractStreetDescriptors(List(streetDescriptor1, blpu("uprn"), lpi("uprn", "usrn"), classification("uprn"), classification("uprn"), organisation("uprn"))).size must beEqualTo(1)
      extractStreetDescriptors(List(streetDescriptor1, blpu("uprn"), lpi("uprn", "usrn"), classification("uprn"), classification("uprn"), organisation("uprn"))) must contain(streetDescriptor1)
    }
  }

  "extractStreetsByUsrn" should {
    "extract all Streets types from a list of generic AddressBase types grouping by USRN" in {
      val street1 = street("usrn")
      val street2 = street("usrn")
      extractStreetsByUsrn(List(street1, street2, streetDescriptor("usrn"), blpu("uprn"), lpi("uprn", "usrn"), classification("uprn"), classification("uprn"), organisation("uprn")))("usrn").size must beEqualTo(2)
      extractStreetsByUsrn(List(street1, street2, streetDescriptor("usrn"), blpu("uprn"), lpi("uprn", "usrn"), classification("uprn"), classification("uprn"), organisation("uprn")))("usrn") must contain(street1, street2)
    }
  }

  /*
  * Check BLPU Status - end date == inactive == ignore
  */

  "blpuIsActive" should {
    "identify active BLPUs" in {
      blpuIsActive(blpu("uprn")) must beTrue
    }

    "identify inactive BLPUs" in {
      blpuIsActive(blpu("uprn").copy(endDate = Some(endDate))) must beFalse
    }
  }

  /**
   * Get most recently updated active AddressBase object from list
   */
  "mostRecentLPIForUprn" should {
    "return LPI for a UPRN" in {
      val lpiForUprn = lpi("uprn", "usrn")
      mostRecentActiveLPIForUprn("uprn", Map("uprn" -> List(lpiForUprn))).get must beEqualTo(lpiForUprn)
    }

    "return None if no LPI for a UPRN" in {
      mostRecentActiveLPIForUprn("none", Map("uprn" -> List(lpi("uprn", "usrn")))) must beEqualTo(None)
    }

    "return active LPI from a list of LPIs for a  UPRN" in {
      val active = lpi("uprn", "usrn1")
      val inActive = lpi("uprn", "usrn2").copy(endDate = Some(endDate))
      mostRecentActiveLPIForUprn("uprn", Map("uprn" -> List(active, inActive))).get must beEqualTo(active)
    }

    "return None if no active LPI from a list of LPIs for a  UPRN" in {
      val inActive1 = lpi("uprn", "usrn1").copy(endDate = Some(endDate))
      val inActive2 = lpi("uprn", "usrn2").copy(endDate = Some(endDate))
      mostRecentActiveLPIForUprn("uprn", Map("uprn" -> List(inActive1, inActive2))) must beEqualTo(None)
    }

    "return most recently updated LPI from a list of active LPIs for a  UPRN" in {
      val lpi1 = lpi("uprn", "usrn1").copy(lastUpdated = new DateTime().minusDays(1))
      val lpi2 = lpi("uprn", "usrn2").copy(lastUpdated = new DateTime().minusDays(2))
      val lpi3 = lpi("uprn", "usrn3").copy(lastUpdated = new DateTime().minusDays(3))
      mostRecentActiveLPIForUprn("uprn", Map("uprn" -> List(lpi3, lpi1, lpi2))).get must beEqualTo(lpi1)
    }
  }

  "mostRecentActiveClassificationForUprn" should {
    "return Classification for a UPRN" in {
      val classificationForUprn = classification("uprn")
      mostRecentActiveClassificationForUprn("uprn", Map("uprn" -> List(classificationForUprn))).get must beEqualTo(classificationForUprn)
    }

    "return None if no Classification for a UPRN" in {
      mostRecentActiveClassificationForUprn("none", Map("uprn" -> List(classification("uprn")))) must beEqualTo(None)
    }

    "return active Classification from a list of Classifications for a  UPRN" in {
      val active = classification("uprn")
      val inActive = classification("uprn").copy(endDate = Some(endDate))
      mostRecentActiveClassificationForUprn("uprn", Map("uprn" -> List(active, inActive))).get must beEqualTo(active)
    }

    "return None if no active LPI from a list of LPIs for a  UPRN" in {
      val inActive1 = classification("uprn").copy(endDate = Some(endDate))
      val inActive2 = classification("uprn").copy(endDate = Some(endDate))
      mostRecentActiveClassificationForUprn("uprn", Map("uprn" -> List(inActive1, inActive2))) must beEqualTo(None)
    }

    "return most recently updated LPI from a list of active LPIs for a  UPRN" in {
      val classification1 = classification("uprn").copy(lastUpdated = new DateTime().minusDays(1))
      val classification2 = classification("uprn").copy(lastUpdated = new DateTime().minusDays(2))
      val classification3 = classification("uprn").copy(lastUpdated = new DateTime().minusDays(3))
      mostRecentActiveClassificationForUprn("uprn", Map("uprn" -> List(classification3, classification2, classification1))).get must beEqualTo(classification1)
    }
  }

  "mostRecentActiveStreetForUsrn" should {
    "return Street for a UPRN" in {
      val streetForUprn = street("usrn")
      mostRecentActiveStreetForUsrn("usrn", Map("usrn" -> List(streetForUprn))).get must beEqualTo(streetForUprn)
    }

    "return None if no Street for a UPRN" in {
      mostRecentActiveStreetForUsrn("none", Map("usrn" -> List(street("usrn")))) must beEqualTo(None)
    }

    "return active Street from a list of Streets for a  UPRN" in {
      val active = street("usrn")
      val inActive = street("usrn").copy(endDate = Some(endDate))
      mostRecentActiveStreetForUsrn("usrn", Map("usrn" -> List(active, inActive))).get must beEqualTo(active)
    }

    "return None if no active LPI from a list of LPIs for a  UPRN" in {
      val inActive1 = street("usrn").copy(endDate = Some(endDate))
      val inActive2 = street("usrn").copy(endDate = Some(endDate))
      mostRecentActiveStreetForUsrn("usrn", Map("usrn" -> List(inActive1, inActive2))) must beEqualTo(None)
    }

    "return most recently updated LPI from a list of active LPIs for a  UPRN" in {
      val street1 = street("usrn").copy(lastUpdated = new DateTime().minusDays(1))
      val street2 = street("usrn").copy(lastUpdated = new DateTime().minusDays(2))
      val street3 = street("usrn").copy(lastUpdated = new DateTime().minusDays(3))
      mostRecentActiveStreetForUsrn("usrn", Map("usrn" -> List(street3, street2, street1))).get must beEqualTo(street1)
    }
  }

  "mostRecentActiveOrganisationForUprn" should {
    "return Organisation for a UPRN" in {
      val organisationForUprn = organisation("uprn")
      mostRecentActiveOrganisationForUprn("uprn", Map("uprn" -> List(organisationForUprn))).get must beEqualTo(organisationForUprn)
    }

    "return None if no Organisation for a UPRN" in {
      mostRecentActiveOrganisationForUprn("none", Map("uprn" -> List(organisation("uprn")))) must beEqualTo(None)
    }

    "return active Organisation from a list of Organisations for a  UPRN" in {
      val active = organisation("uprn")
      val inActive = organisation("uprn").copy(endDate = Some(endDate))
      mostRecentActiveOrganisationForUprn("uprn", Map("uprn" -> List(active, inActive))).get must beEqualTo(active)
    }

    "return None if no active LPI from a list of LPIs for a  UPRN" in {
      val inActive1 = organisation("uprn").copy(endDate = Some(endDate))
      val inActive2 = organisation("uprn").copy(endDate = Some(endDate))
      mostRecentActiveOrganisationForUprn("uprn", Map("uprn" -> List(inActive1, inActive2))) must beEqualTo(None)
    }

    "return most recently updated LPI from a list of active LPIs for a  UPRN" in {
      val organisation1 = organisation("uprn").copy(lastUpdated = new DateTime().minusDays(1))
      val organisation2 = organisation("uprn").copy(lastUpdated = new DateTime().minusDays(2))
      val organisation3 = organisation("uprn").copy(lastUpdated = new DateTime().minusDays(3))
      mostRecentActiveOrganisationForUprn("uprn", Map("uprn" -> List(organisation3, organisation2, organisation1))).get must beEqualTo(organisation1)
    }
  }


  /**
   * Convert a street object and a street description onto a single representative object
   */
  "toStreetWithDescription" should {
    "create a street description object from a valid street and street description" in {
      val description = streetDescriptor("usrn")
      val st = street("usrn")
      val streets = Map("usrn" -> List(st))
      val streetWrapper = toStreetWithDescription("filename", streets, description)

      streetWrapper must not(beEqualTo(None))
      streetWrapper.get.file must beEqualTo("filename")
      streetWrapper.get.usrn must beEqualTo(st.usrn)
      streetWrapper.get.streetDescription must beEqualTo(description.streetDescription)
      streetWrapper.get.localityName must beEqualTo(description.localityName)
      streetWrapper.get.townName must beEqualTo(description.townName)
      streetWrapper.get.administrativeArea must beEqualTo(description.administrativeArea)
      streetWrapper.get.recordType.get must beEqualTo(st.recordType.get.toString)
      streetWrapper.get.state.get must beEqualTo(st.state.get.toString)
      streetWrapper.get.surface.get must beEqualTo(st.surface.get.toString)
      streetWrapper.get.classification.get must beEqualTo(st.classification.get.toString)
    }

    "create a street description object from a valid street and street description - with all optional fields as None" in {

      val description = StreetDescriptor("usrn", "description", None, None, "area")
      val st = Street("usrn", None, None, None, None, startDate, None, lastUpdatedDate)

      val streets = Map("usrn" -> List(st))
      val streetWrapper = toStreetWithDescription("filename", streets, description)

      streetWrapper must not(beEqualTo(None))
      streetWrapper.get.file must beEqualTo("filename")
      streetWrapper.get.usrn must beEqualTo(st.usrn)
      streetWrapper.get.streetDescription must beEqualTo(description.streetDescription)
      streetWrapper.get.localityName must beEqualTo(None)
      streetWrapper.get.townName must beEqualTo(None)
      streetWrapper.get.administrativeArea must beEqualTo(description.administrativeArea)
      streetWrapper.get.recordType must beEqualTo(None)
      streetWrapper.get.state must beEqualTo(None)
      streetWrapper.get.surface must beEqualTo(None)
      streetWrapper.get.classification must beEqualTo(None)
    }

    "should return none if no street with matching usrn with streetdescription - no matching key" in {
      val description = streetDescriptor("usrn1")
      val streets = Map("usrn2" -> List(street("usrn2")))
      toStreetWithDescription("filename", streets, description) must beEqualTo(None)
    }

    "should return none if no street with matching usrn with streetdescription - empty list of streets" in {
      val description = streetDescriptor("usrn1")
      val streets = Map("usrn1" -> List.empty[Street])
      toStreetWithDescription("filename", streets, description) must beEqualTo(None)
    }

    "should return none if no active street with matching usrn with streetdescription" in {
      val description = streetDescriptor("usrn1")
      val st = street("usrn").copy(endDate = Some(endDate))
      val streets = Map("usrn" -> List(st))
      toStreetWithDescription("filename", streets, description) must beEqualTo(None)
    }
  }

  /**
   * Create a wrapper to contain the active core AddressBase objects
   */
  "toAddressBaseWrapper" should {
    "create a wrapper object containing linked BLPU, LPI, Classification and Organisation objects" in {
      val b = blpu("uprn")
      val l = lpi("uprn", "usrn")
      val c = classification("uprn")
      val o = organisation("uprn")
      val addressBaseWrapper = toAddressBaseWrapper("filename", b, Map("uprn" -> List(l)), Map("uprn" -> List(c)), Map("uprn" -> List(o))).get
      addressBaseWrapper.blpu must beEqualTo(b)
      addressBaseWrapper.lpi must beEqualTo(l)
      addressBaseWrapper.classification must beEqualTo(c)
      addressBaseWrapper.organisation.get must beEqualTo(o)
    }

    "create a wrapper object containing linked BLPU, LPI, Classification objects and no Organisation if none available" in {
      val b = blpu("uprn")
      val l = lpi("uprn", "usrn")
      val c = classification("uprn")
      val addressBaseWrapper = toAddressBaseWrapper("filename", b, Map("uprn" -> List(l)), Map("uprn" -> List(c)), Map.empty[String, List[Organisation]]).get
      addressBaseWrapper.blpu must beEqualTo(b)
      addressBaseWrapper.lpi must beEqualTo(l)
      addressBaseWrapper.classification must beEqualTo(c)
      addressBaseWrapper.organisation must beEqualTo(None)
    }

    "return None if BLPU is inactive" in {
      val b = blpu("uprn").copy(endDate = Some(endDate))
      val l = lpi("uprn", "usrn")
      val c = classification("uprn")
      val o = organisation("uprn")
      toAddressBaseWrapper("filename", b, Map("uprn" -> List(l)), Map("uprn" -> List(c)), Map("uprn" -> List(o))) must beEqualTo(None)
    }

    "return None if no LPI for that UPRN - empty map for key" in {
      val b = blpu("uprn")
      val l = lpi("different-uprn", "usrn")
      val c = classification("uprn")
      val o = organisation("uprn")
      toAddressBaseWrapper("filename", b, Map("different-uprn" -> List(l)), Map("uprn" -> List(c)), Map("uprn" -> List(o))) must beEqualTo(None)
    }

    "return None if no LPI for that UPRN - empty list associated with key" in {
      val b = blpu("uprn")
      val c = classification("uprn")
      val o = organisation("uprn")
      toAddressBaseWrapper("filename", b, Map("uprn" -> List.empty[LPI]), Map("uprn" -> List(c)), Map("uprn" -> List(o))) must beEqualTo(None)
    }

    "return None if no active LPI for that UPRN" in {
      val b = blpu("uprn")
      val l = lpi("uprn", "usrn").copy(endDate = Some(endDate))
      val c = classification("uprn")
      val o = organisation("uprn")
      toAddressBaseWrapper("filename", b, Map("uprn" -> List(l)), Map("uprn" -> List(c)), Map("uprn" -> List(o))) must beEqualTo(None)
    }

    "return most recently updated active LPI for that UPRN" in {
      val b = blpu("uprn")
      val l1 = lpi("uprn", "usrn").copy(lastUpdated = new DateTime().minusDays(50))
      val l2 = lpi("uprn", "usrn").copy(lastUpdated = new DateTime().minusDays(100))
      val c = classification("uprn")
      val o = organisation("uprn")
      val addressBaseWrapper = toAddressBaseWrapper("filename", b, Map("uprn" -> List(l1,l2)), Map("uprn" -> List(c)), Map("uprn" -> List(o))).get
      addressBaseWrapper.lpi must beEqualTo(l1)
    }

    "return None if no Classification for that UPRN - empty list" in {
      val b = blpu("uprn")
      val l = lpi("uprn", "usrn")
      val o = organisation("uprn")
      toAddressBaseWrapper("filename", b, Map("uprn" -> List(l)), Map("uprn" -> List.empty[Classification]), Map("uprn" -> List(o))) must beEqualTo(None)
    }

    "return None if no Classification for that UPRN - no matching uprn key" in {
      val b = blpu("uprn")
      val l = lpi("uprn", "usrn")
      val c = classification("different uprn")
      val o = organisation("uprn")
      toAddressBaseWrapper("filename", b, Map("uprn" -> List(l)), Map("different uprn" -> List(c)), Map("uprn" -> List(o))) must beEqualTo(None)
    }
  }


  /**
   * Helpers
   */
  private lazy val startDate = new DateTime().minusDays(100)
  private lazy val lastUpdatedDate = new DateTime().minusDays(50)
  private lazy val endDate = new DateTime().minusDays(50)

  private def blpu(uprn: String) = BLPU(
    uprn,
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


  private def lpi(uprn: String, usrn: String) = LPI(
    uprn,
    usrn,
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


  private def street(usrn: String) = Street(usrn, Some(StreetRecordTypeCode.numberedStreet), Some(StreetStateCode.open), Some(StreetSurfaceCode.mixed), Some(StreetClassificationCode.allVehicles), startDate, None, lastUpdatedDate)

  private def streetDescriptor(usrn: String) = StreetDescriptor(usrn, "description", Some("locality"), Some("town"), "admin area")

  private def classification(uprn: String) = Classification(uprn, "code", startDate, None, lastUpdatedDate, "primaryUse", Some("secondaryUse"))

  private def organisation(uprn: String) = Organisation(uprn, "organisation", startDate, None, lastUpdatedDate)
}