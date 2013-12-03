package uk.gov.gds.model

import org.specs2.mutable.Specification
import uk.gov.gds.model.CodeLists._
import org.joda.time.DateTime
import scala.Some
import org.specs2.mock.Mockito
import uk.gov.gds.testutils.ReporterTestUtils._

class AddressBuilderTests extends Specification with Mockito {

  import AddressBuilder._
  import uk.gov.gds.model.formatters._

  val mongoConnection = mock[uk.gov.gds.MongoConnection]
  mongoConnection.streetForUsrn(anyString) returns Some(streetWithDescription)

  "The address builder" should {

    "not create an address if no street available for the usrn" in {
      val file = randomFilename

      val mongoConnectionWithNoStreet = mock[uk.gov.gds.MongoConnection]
      mongoConnectionWithNoStreet.streetForUsrn(anyString) returns None

      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu, lpi, classification, Some(organisation)))(Some(mongoConnectionWithNoStreet), file) must beEqualTo(None)

      reportLineToTest(file) must not be None
      reportLineToTest(file).get must contain("no-street-for-blpu")
    }

    "not create an address if no eligible BLPU (logical status not approved)" in {
      val file = randomFilename
      geographicAddressToSimpleAddress(AddressBaseWrapper(blpuProvisonal, lpi, classification, Some(organisation)))(Some(mongoConnection), file) must beEqualTo(None)

      reportLineToTest(file) must not be None
      reportLineToTest(file).get must contain("invalid-blpu")
    }.pendingUntilFixed("Adding all logical states currently")

    "not create an address if no eligible BLPU ( state not in use)" in {
      val file = randomFilename
      geographicAddressToSimpleAddress(AddressBaseWrapper(invalidBlpuDueToBlpuStatus, lpi, classification, Some(organisation)))(Some(mongoConnection), file) must beEqualTo(None)

      reportLineToTest(file) must not be None
      reportLineToTest(file).get must contain("invalid-blpu")
    }.pendingUntilFixed("Adding all BLPU states currently")

    "not create an address if no eligible BLPU ( end date present)" in {
      val file = randomFilename

      geographicAddressToSimpleAddress(AddressBaseWrapper(invalidBlpuDueToEndDates, lpi, classification, Some(organisation)))(Some(mongoConnection), file) must beEqualTo(None)

      reportLineToTest(file) must not be None
      reportLineToTest(file).get must contain("invalid-blpu")
    }

    "not create an address if no eligible BLPU ( can't receieve post)" in {
      val file = randomFilename
      geographicAddressToSimpleAddress(AddressBaseWrapper(invalidBlpuDueToPostalStatus, lpi, classification, Some(organisation)))(Some(mongoConnection), file) must beEqualTo(None)
      reportLineToTest(file) must not be None
      reportLineToTest(file).get must contain("invalid-blpu")
    }.pendingUntilFixed("Adding all non-postal addresses states currently")

    "include the postcode from the BLPU all lowercased on the root address object" in {
      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu.copy(postcode = "SE45 0PP"), lpi, classification, Some(organisation)))(Some(mongoConnection), randomFilename).get.postcode must beEqualTo("se450pp")
    }

    "include the local custodian code from the BLPU on the root address object as gss code" in {
      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu.copy(localCustodianCode = "1234"), lpi, classification, Some(organisation)))(Some(mongoConnection), randomFilename).get.gssCode must beEqualTo("1234")
    }

    "include the pao text from the LPI on the root address object as houseName" in {
      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu, lpi.copy(paoText = "house name"), classification, Some(organisation)))(Some(mongoConnection), randomFilename).get.houseName.get must beEqualTo("house name")
    }

    "not include house name if the pao text from the LPI is absent" in {
      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu, lpi.copy(paoText = None), classification, Some(organisation)))(Some(mongoConnection), randomFilename).get.houseName must beEqualTo(None)
    }

    "include house number generated from the LPI PAO fields on the root address object" in {
      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu,
        lpi.copy(paoStartNumber = Some("99"), paoStartSuffix = Some("A"), paoEndNumber = Some("100"), paoEndSuffix = Some("Z"), saoStartNumber = Some("9"), saoStartSuffix = Some("A"), saoEndNumber = Some("10"), saoEndSuffix = Some("B"))
        , classification, Some(organisation)))(Some(mongoConnection), randomFilename).get.houseNumber.get must beEqualTo("99A-100Z")
    }

    "not include house number if no LPI PAO fields - ignoring SAO fields" in {
      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu,
        lpi.copy(paoStartNumber = None, paoStartSuffix = None, paoEndNumber = None, paoEndSuffix = None, saoStartNumber = Some("9"), saoStartSuffix = Some("A"), saoEndNumber = Some("10"), saoEndSuffix = Some("B"))
        , classification, Some(organisation)))(Some(mongoConnection), randomFilename).get.houseNumber must beEqualTo(None)
    }

    "include the postcode from the BLPU as supplied on the presentation address object" in {
      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu.copy(postcode = "SE45 0PP"), lpi, classification, Some(organisation)))(Some(mongoConnection), randomFilename).get.presentation.postcode must beEqualTo("SE45 0PP")
    }

    "construct suffixes - if start and end they should be seperated by a dash is both present" in {
      formatStartAndEndNumbersAndSuffixes(Some("10"), Some("A"), Some("11"), Some("B")).get must beEqualTo("10A-11B")
    }

    "construct suffixes - if start and end they should be seperated by a dash is both present - with no suffixes" in {
      formatStartAndEndNumbersAndSuffixes(Some("10"), None, Some("11"), None).get must beEqualTo("10-11")
    }

    "construct suffixes - if start and end they should be seperated by a dash is both present - with only start suffix" in {
      formatStartAndEndNumbersAndSuffixes(Some("10"), Some("A"), Some("11"), None).get must beEqualTo("10A-11")
    }

    "construct suffixes - if start and end they should be seperated by a dash is both present - with only end suffix" in {
      formatStartAndEndNumbersAndSuffixes(Some("10"), None, Some("11"), Some("B")).get must beEqualTo("10-11B")
    }

    "construct suffixes - start only with suffix" in {
      formatStartAndEndNumbersAndSuffixes(Some("10"), Some("A"), None, None).get must beEqualTo("10A")
    }

    "construct suffixes - start only with no suffix" in {
      formatStartAndEndNumbersAndSuffixes(Some("10"), None, None, None).get must beEqualTo("10")
    }

    "construct suffixes - start only with suffix ignores end suffix if no end number" in {
      formatStartAndEndNumbersAndSuffixes(Some("10"), Some("A"), None, Some("B")).get must beEqualTo("10A")
    }

    "construct suffixes - should be None if no suffixes / numbers present" in {
      formatStartAndEndNumbersAndSuffixes(None, None, None, None) must beEqualTo(None)
    }

    "construct suffixes - should be None if only end suffixes and numbers present" in {
      formatStartAndEndNumbersAndSuffixes(None, None, Some("10"), Some("B")) must beEqualTo(None)
    }

    "construct suffixes - should be None if only end suffixes present" in {
      formatStartAndEndNumbersAndSuffixes(None, None, None, Some("B")) must beEqualTo(None)
    }

    "construct suffixes - should be None if only end numbers present" in {
      formatStartAndEndNumbersAndSuffixes(None, None, Some("10"), None) must beEqualTo(None)
    }

    "construct suffixes - should be None if only start suffix present" in {
      formatStartAndEndNumbersAndSuffixes(None, Some("B"), None, None) must beEqualTo(None)
    }

    "should transform all the SAO fields plus PAO Text into a string excluding the pao fields fornumber and suffix" in {
      constructPropertyFrom(lpi).get must beEqualTo("sao start numbersao start suffix-sao end numbersao end suffix sao text pao text")
    }

    "should transform all the SAO fields supplied plus PAO Text into a string" in {
      constructPropertyFrom(lpiWithSaoStartAndPaoTextAndNoSaoEndOrSaoText).get must beEqualTo("sao start numbersao start suffix pao text")
    }

    "should transform all the SAO and PAO fields successfully when no suffix" in {
      constructPropertyFrom(lpiWithNoSuffix).get must beEqualTo("sao start number-sao end number sao text pao text")
    }

    "should transform all the SAO and PAO fields excluding the suffixs when no numbers" in {
      constructPropertyFrom(lpiWithNoNumbers).get must beEqualTo("sao text pao text")
    }

    "should transform all missing sao and pao text fields into an empty string that returns true to isEmpty" in {
      constructPropertyFrom(lpiWithNoSaoFieldsAndNoPaoText) must beEqualTo(None)
    }

    "should format a street address from the pao start and end numbers on an LPI" in {
      constructStreetAddressPrefixFrom(lpi).get must beEqualTo("pao start numberpao start suffix-pao end numberpao end suffix")
    }

    "should return None for a street address if no pao start number on an LPI" in {
      constructStreetAddressPrefixFrom(lpi.copy(paoStartNumber = None)) must beEqualTo(None)
    }

    "should construct a location object from the x and y of a blpu" in {
      location(validBlpu).x must beEqualTo(1.1)
      location(validBlpu).y must beEqualTo(2.2)
    }

    "should construct a street address from the LPI and the street description" in {
      constructStreetAddressFrom(
        lpi.copy(paoStartNumber = Some("10"), paoStartSuffix = Some("a"), paoEndNumber = Some("11"), paoEndSuffix = Some("b")), streetWithDescription.copy(streetDescription = "The Street")
      ).get must beEqualTo("10a-11b The Street")
    }

    "should construct a street address from the LPI with no pao start suffix and the street description" in {
      constructStreetAddressFrom(
        lpi.copy(paoStartNumber = Some("10"), paoStartSuffix = None, paoEndNumber = Some("11"), paoEndSuffix = Some("b")), streetWithDescription.copy(streetDescription = "The Street")
      ).get must beEqualTo("10-11b The Street")
    }

    "should construct a street address from the LPI with no pao end and the street description" in {
      constructStreetAddressFrom(
        lpi.copy(paoStartNumber = Some("10"), paoStartSuffix = None, paoEndNumber = None, paoEndSuffix = None), streetWithDescription.copy(streetDescription = "The Street")
      ).get must beEqualTo("10 The Street")
    }

    "valid blpus should be marked as valid" in {
      isValidBLPU(validBlpu) must beTrue
    }

    "invalid blpu due to recieve post must be marked invalid" in {
      isValidBLPU(invalidBlpuDueToPostalStatus) must beFalse
    }.pendingUntilFixed("allowing non-postal addreses at the momemnt")

    "invalid blpu due to blpu status must be marked invalid" in {
      isValidBLPU(invalidBlpuDueToLogicalStatus) must beFalse
    }.pendingUntilFixed("allowing all logical states at the momemnt")

    "invalid blpu due to logical state must be marked invalid" in {
      isValidBLPU(invalidBlpuDueToBlpuStatus) must beFalse
    }.pendingUntilFixed("allowing all statues at the momemnt")

    "invalid blpu due to end date must be marked invalid" in {
      isValidBLPU(invalidBlpuDueToEndDates) must beFalse
    }

    "make a details object containing the correct classification information" in {
      details(AddressBaseWrapper(validBlpu, lpi, classification.copy(classificationCode = "R1"), None)).classification must beEqualTo("R1")
      details(AddressBaseWrapper(validBlpu, lpi, classification.copy(classificationCode = "R1"), None)).isResidential must beEqualTo(true)
      details(AddressBaseWrapper(validBlpu, lpi, classification.copy(classificationCode = "C1"), None)).isCommercial must beEqualTo(true)
    }

    "make a detail object that correctly interprets the postal address" in {
      details(AddressBaseWrapper(validBlpu.copy(receivesPost = "N"), lpi, classification, None)).isPostalAddress must beEqualTo(false)
    }

    "make a detail object that correctly set the usrn" in {
      details(AddressBaseWrapper(validBlpu, lpi.copy(usrn = "USRN"), classification, None)).usrn must beEqualTo("USRN")
    }

    "make a detail object that correctly contains the updated and created dates from the BLPU" in {
      details(AddressBaseWrapper(validBlpu.copy(startDate = startDate), lpi, classification, None)).blpuCreatedAt must beEqualTo(startDate)
      details(AddressBaseWrapper(validBlpu.copy(lastUpdated = lastUpdatedDate), lpi, classification, None)).blpuUpdatedAt must beEqualTo(lastUpdatedDate)
    }

    "make a detail object that correctly contains the BLPU logical status and state codes" in {
      details(AddressBaseWrapper(validBlpu.copy(logicalState = Some(LogicalStatusCode.approved)), lpi, classification, None)).state.get must beEqualTo("approved")
      details(AddressBaseWrapper(validBlpu.copy(blpuState = Some(BlpuStateCode.inUse)), lpi, classification, None)).status.get must beEqualTo("inUse")
    }

    "make an address object with all the fields set up correctly" in {
      val address = geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu, lpi, classification, None))(Some(mongoConnection), randomFilename).get

      /* base object */
      address.gssCode must beEqualTo("1234")
      address.postcode must beEqualTo("postcode")
      address.houseName.get must beEqualTo("pao text")
      address.houseNumber.get must beEqualTo("pao start numberpao start suffix-pao end numberpao end suffix")

      /* details */
      address.details.blpuCreatedAt must beEqualTo(startDate)
      address.details.blpuUpdatedAt must beEqualTo(lastUpdatedDate)
      address.details.classification must beEqualTo("code")
      address.details.isPostalAddress must beEqualTo(true)
      address.details.isResidential must beEqualTo(false)
      address.details.usrn must beEqualTo("usrn")
      address.details.state.get must beEqualTo("approved")
      address.details.status.get must beEqualTo("inUse")

      /* location */
      address.location.x must beEqualTo(1.1)
      address.location.y must beEqualTo(2.2)

      /* presentation */
      address.presentation.postcode must beEqualTo("postcode")
      address.presentation.uprn must beEqualTo("uprn")
      address.presentation.town.get must beEqualTo("town")
      address.presentation.area.get must beEqualTo("area")
      address.presentation.locality.get must beEqualTo("locality")
      address.presentation.streetAddress.get must beEqualTo("pao start numberpao start suffix-pao end numberpao end suffix street name")
      address.presentation.property.get must beEqualTo("sao start numbersao start suffix-sao end numbersao end suffix sao text pao text")
    }

    "make an address object with no street description if street name is not official" in {
      val mongoConnectionWithNoStreet = mock[uk.gov.gds.MongoConnection]
      mongoConnectionWithNoStreet.streetForUsrn(anyString) returns Some(streetWithDescription.copy(recordType = Some("unoffical")))
      val address = geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu, lpi, classification, None))(Some(mongoConnectionWithNoStreet), randomFilename).get

      /* base object */
      address.gssCode must beEqualTo("1234")
      address.postcode must beEqualTo("postcode")
      address.houseName.get must beEqualTo("pao text")
      address.houseNumber.get must beEqualTo("pao start numberpao start suffix-pao end numberpao end suffix")

      /* details */
      address.details.blpuCreatedAt must beEqualTo(startDate)
      address.details.blpuUpdatedAt must beEqualTo(lastUpdatedDate)
      address.details.classification must beEqualTo("code")
      address.details.isPostalAddress must beEqualTo(true)
      address.details.isResidential must beEqualTo(false)
      address.details.usrn must beEqualTo("usrn")
      address.details.state.get must beEqualTo("approved")
      address.details.status.get must beEqualTo("inUse")

      /* location */
      address.location.x must beEqualTo(1.1)
      address.location.y must beEqualTo(2.2)

      /* presentation */
      address.presentation.postcode must beEqualTo("postcode")
      address.presentation.uprn must beEqualTo("uprn")
      address.presentation.town.get must beEqualTo("town")
      address.presentation.area.get must beEqualTo("area")
      address.presentation.locality.get must beEqualTo("locality")
      address.presentation.streetAddress must beEqualTo(None)
      address.presentation.property.get must beEqualTo("sao start numbersao start suffix-sao end numbersao end suffix sao text pao text")
    }
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

  private lazy val invalidBlpuDueToPostalStatus = validBlpu.copy(receivesPost = "N")
  private lazy val invalidBlpuDueToLogicalStatus = validBlpu.copy(logicalState = Some(LogicalStatusCode.historical))
  private lazy val invalidBlpuDueToBlpuStatus = validBlpu.copy(blpuState = Some(BlpuStateCode.noLongerExists))
  private lazy val invalidBlpuDueToEndDates = validBlpu.copy(endDate = Some(new DateTime()))
  private lazy val blpuProvisonal = validBlpu.copy(logicalState = Some(LogicalStatusCode.provisional))

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

  private lazy val lpiWithNoSuffix = lpi.copy(saoStartSuffix = None, saoEndSuffix = None, paoStartSuffix = None, paoEndSuffix = None)
  private lazy val lpiWithNoNumbers = lpi.copy(saoStartNumber = None, saoEndNumber = None, paoStartNumber = None, paoEndNumber = None)

  private lazy val lpiWithSaoStartAndPaoTextAndNoSaoEndOrSaoText = lpi.copy(saoStartNumber = Some("sao start number"), saoStartSuffix = Some("sao start suffix"), saoEndNumber = None, saoEndSuffix = None, saoText = None, paoText = Some("pao text"))

  private lazy val lpiWithNoSaoFieldsAndNoPaoText = lpi.copy(
    saoStartNumber = None, saoStartSuffix = None, saoEndNumber = None, saoEndSuffix = None, saoText = None,
    paoStartNumber = None, paoStartSuffix = None, paoEndNumber = None, paoEndSuffix = None, paoText = None
  )


  private lazy val classification = Classification("uprn", "code", startDate, None, lastUpdatedDate)
  private lazy val organisation = Organisation("uprn", "organisation", startDate, None, lastUpdatedDate)
  private lazy val streetWithDescription = StreetWithDescription("usrn", "street name", "locality", "town", "area", Some("officiallyDesignated"), Some("state"), Some("code"), Some("classification"))


}
