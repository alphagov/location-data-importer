package uk.gov.gds.location.importer.conversions

import org.specs2.mutable.Specification
import org.specs2.mock.Mockito
import uk.gov.gds.location.importer.model._
import uk.gov.gds.location.importer.helpers.TestHelpers._
import uk.gov.gds.location.importer.model.AddressBaseWrapper
import scala.Some

class AddressBaseToLocateConvertorTests extends Specification with Mockito {

  import AddressBaseToLocateConvertor._
  import formatters._

  sequential


  "Address conversions" should {

    "not create an address if no street available for the usrn" in {
      val addressWrapper = AddressBaseWrapper(blpu("blpu"), lpi("uprn", "usrn"), classification("uprn"), None, None)
      toLocateAddress(addressWrapper, "filename") must beEqualTo(None)
    }

    "not create an address if street available for the usrn but no authority for custodian code" in {
      AllTheStreets.add(List(streetWithDescription("filename", streetDescriptor("usrn"), street("usrn"))))
      val addressWrapper = AddressBaseWrapper(blpu("blpu").copy(localCustodianCode = "doesn't exist"), lpi("uprn", "usrn"), classification("uprn"), None, None)
      toLocateAddress(addressWrapper, "filename") must beEqualTo(None)
    }

    "create an address if street and custodian code available for the address" in {
      AllTheStreets.add(List(streetWithDescription("filename", streetDescriptor("usrn"), street("usrn"))))
      val addressWrapper = AddressBaseWrapper(blpu("blpu").copy(localCustodianCode = "9051"), lpi("uprn", "usrn"), classification("uprn"), None, None)
      toLocateAddress(addressWrapper, "filename").isDefined must beTrue
    }

    "create an address with correct root fields derived from custodian code and blpu" in {
      AllTheStreets.add(List(streetWithDescription("filename", streetDescriptor("usrn"), street("usrn"))))
      val addressWrapper = AddressBaseWrapper(blpu("blpu").copy(postcode = "SW11 2DR", uprn = "123456", localCustodianCode = "9051"), lpi("uprn", "usrn"), classification("uprn"), None, None)
      toLocateAddress(addressWrapper, "filename").get.postcode must beEqualTo("sw112dr")
      toLocateAddress(addressWrapper, "filename").get.uprn must beEqualTo("123456")
      toLocateAddress(addressWrapper, "filename").get.gssCode must beEqualTo("S12000033")
      toLocateAddress(addressWrapper, "filename").get.country must beEqualTo("Scotland")
    }
  }

  "GssCode comparison" should {
    "be able to compare a custodian code derived gsscode with one from code point" in {
      checkGssCodeWithCustodianCode(blpu("uprn").copy(localCustodianCode = "9052"), "S12000034", "filename") must beEqualTo("S12000034")
    }

    "be able to compare a custodian code derived gsscode with one from code point - returning Code point version if no resolved custodian code" in {
      checkGssCodeWithCustodianCode(blpu("uprn").copy(localCustodianCode = "ferfew"), "S12000033", "filename") must beEqualTo("S12000033")
    }

    "be able to compare a custodian code derived gsscode with one from code point - returning custodian code version if disagreement" in {
      checkGssCodeWithCustodianCode(blpu("uprn").copy(localCustodianCode = "9052"), "S1200", "filename") must beEqualTo("S12000034")
    }
  }

  "Orderings" should {
    "build full set of ordering information from sao and pao fields in LPI" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = Some("x"),
        paoEndNumber = Some("4"),
        paoEndSuffix = Some("y"),
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )
      val o = ordering(AddressBaseWrapper(blpu("uprn"), l, classification("uprn"), None, None))
      o.saoStartNumber.get must beEqualTo(1)
      o.saoStartSuffix.get must beEqualTo("a")
      o.saoEndNumber.get must beEqualTo(2)
      o.saoEndSuffix.get must beEqualTo("b")
      o.paoStartNumber.get must beEqualTo(3)
      o.paoStartSuffix.get must beEqualTo("x")
      o.paoEndNumber.get must beEqualTo(4)
      o.paoEndSuffix.get must beEqualTo("y")
      o.saoText.get must beEqualTo("saotext")
      o.paoText.get must beEqualTo("paotext")
    }

    "build empty set of ordering information from empty sao and pao fields in LPI" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = None,
        saoStartSuffix = None,
        saoEndNumber = None,
        saoEndSuffix = None,
        paoStartNumber = None,
        paoStartSuffix = None,
        paoEndNumber = None,
        paoEndSuffix = None,
        saoText = None,
        paoText = None
      )
      val o = ordering(AddressBaseWrapper(blpu("uprn"), l, classification("uprn"), None, None))
      o.saoStartNumber must beEqualTo(None)
      o.saoStartSuffix must beEqualTo(None)
      o.saoEndNumber must beEqualTo(None)
      o.saoEndSuffix must beEqualTo(None)
      o.paoStartNumber must beEqualTo(None)
      o.paoStartSuffix must beEqualTo(None)
      o.paoEndNumber must beEqualTo(None)
      o.paoEndSuffix must beEqualTo(None)
      o.saoText must beEqualTo(None)
      o.paoText must beEqualTo(None)
    }

    "build empty set of ordering numeric information from non-numeric strings in sao and pao number fields in LPI" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("thing"),
        saoEndNumber = Some("thing"),
        paoStartNumber = Some("thing"),
        paoEndNumber = Some("thing"),
        saoText = None,
        paoText = None
      )
      val o = ordering(AddressBaseWrapper(blpu("uprn"), l, classification("uprn"), None, None))
      o.saoStartNumber must beEqualTo(None)
      o.saoEndNumber must beEqualTo(None)
      o.paoStartNumber must beEqualTo(None)
      o.paoEndNumber must beEqualTo(None)
      o.saoText must beEqualTo(None)
      o.paoText must beEqualTo(None)
    }

    "build set of lower cased and white space stripped ordering text information from sao and pao text fields in LPI" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = None,
        saoEndNumber = None,
        paoStartNumber = None,
        paoEndNumber = None,
        saoText = Some("CAPITALS AND WHITE SPACE"),
        paoText = Some("CAPITALS AND WHITE SPACE")
      )
      val o = ordering(AddressBaseWrapper(blpu("uprn"), l, classification("uprn"), None, None))
      o.saoStartNumber must beEqualTo(None)
      o.saoEndNumber must beEqualTo(None)
      o.paoStartNumber must beEqualTo(None)
      o.paoEndNumber must beEqualTo(None)
      o.saoText.get must beEqualTo("capitalsandwhitespace")
      o.paoText.get must beEqualTo("capitalsandwhitespace")
    }
  }

  "Location" should {
    "have lat long set correctly" in {
      val b = blpu("uprn").copy(lat = 1.1, long = 2.2)
      val l = location(b)
      l.lat should beEqualTo(1.1)
      l.long should beEqualTo(2.2)
    }
  }

  "Sentence Case conversions" should {
    "convert strings to sentence case" in {
      toSentenceCase(None) must beEqualTo(None)
      toSentenceCase(Some("this is not sentence case")).get must beEqualTo("This Is Not Sentence Case")
      toSentenceCase(Some("THIS IS NOT SENTENCE CASE")).get must beEqualTo("This Is Not Sentence Case")
      toSentenceCase(Some("This is not Sentence case")).get must beEqualTo("This Is Not Sentence Case")
      toSentenceCase(Some("this is nOT senTENce case")).get must beEqualTo("This Is Not Sentence Case")
      toSentenceCase(Some("thisisnOTsenTENcecase")).get must beEqualTo("Thisisnotsentencecase")
    }
  }

  "Strip whitespace" should {
    "remove in string whitespace" in {
      stripAllWhitespace("this has whitespace") must beEqualTo("thishaswhitespace")
    }
    "remove leading whitespace" in {
      stripAllWhitespace("  this has whitespace") must beEqualTo("thishaswhitespace")
    }
    "remove trailing whitespace" in {
      stripAllWhitespace("this has whitespace    ") must beEqualTo("thishaswhitespace")
    }
    "remove newline whitespace" in {
      stripAllWhitespace("this has whitespace  \n" +
        "" +
        "" +
        "  ") must beEqualTo("thishaswhitespace")
    }
  }

  "numbers and suffixes" should {
    "be all included if all present" in {
      formatStartAndEndNumbersAndSuffixes(Some("1"), Some("a"), Some("2"), Some("b")).get must beEqualTo("1a-2b")
    }
    "be none if no numbers/suffixes" in {
      formatStartAndEndNumbersAndSuffixes(None, None, None, None).isDefined must beFalse
    }
    "include only numbers if no suffixes" in {
      formatStartAndEndNumbersAndSuffixes(Some("1"), None, Some("2"), None).get must beEqualTo("1-2")
    }
    "be none if only suffixes" in {
      formatStartAndEndNumbersAndSuffixes(None, Some("a"), None, Some("b")).isDefined must beFalse
    }
    "include only start fields if no end fields" in {
      formatStartAndEndNumbersAndSuffixes(Some("1"), None, None, None).get must beEqualTo("1")
      formatStartAndEndNumbersAndSuffixes(Some("1"), Some("a"), None, None).get must beEqualTo("1a")
    }
    "be end numbers/suffixes if only end numbers/suffixes" in {
      formatStartAndEndNumbersAndSuffixes(None, None, Some("1"), None).get must beEqualTo("1")
      formatStartAndEndNumbersAndSuffixes(None, None, Some("1"), Some("a")).get must beEqualTo("1a")
    }
  }

  "constructStreetAddressPrefixFrom" should {
    "correctly create street number from fully populated LPI" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = Some("c"),
        paoEndNumber = Some("4"),
        paoEndSuffix = Some("d"),
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )

      constructStreetAddressPrefixFrom(l).get must beEqualTo("3c-4d")
    }

    "correctly create street number from only pao numbers populated LPI" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = None,
        paoEndNumber = Some("4"),
        paoEndSuffix = None,
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )

      constructStreetAddressPrefixFrom(l).get must beEqualTo("3-4")
    }

    "correctly create street number from only start pao numbers populated LPI" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = None,
        paoEndNumber = None,
        paoEndSuffix = None,
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )

      constructStreetAddressPrefixFrom(l).get must beEqualTo("3")
    }

    "correctly create street number from only end pao numbers populated LPI" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = None,
        paoStartSuffix = None,
        paoEndNumber = Some("4"),
        paoEndSuffix = None,
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )

      constructStreetAddressPrefixFrom(l).get must beEqualTo("4")
    }

    "correctly create none for street number from no pao populated LPI" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = None,
        paoStartSuffix = None,
        paoEndNumber = None,
        paoEndSuffix = None,
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )

      constructStreetAddressPrefixFrom(l).isDefined must beFalse
    }
  }

  "construct street address" should {
    "be populated if officially designated or numbered street" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = Some("c"),
        paoEndNumber = Some("4"),
        paoEndSuffix = Some("d"),
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )
      val dp = deliveryPoint("uprn")
      val sd1 = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(recordType = Some("officiallyDesignated"))
      constructStreetAddressFrom(l, sd1, Some(dp)).isDefined must beTrue
      val sd2 = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(recordType = Some("numberedStreet"))
      constructStreetAddressFrom(l, sd2, Some(dp)).isDefined must beTrue
    }

    "be populated from delivery point if not officially designated or numbered street" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = Some("c"),
        paoEndNumber = Some("4"),
        paoEndSuffix = Some("d"),
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )
      val dp = deliveryPoint("uprn")
      val sd1 = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(recordType = Some("unofficialStreetDescription"))
      constructStreetAddressFrom(l, sd1, Some(dp)).isDefined must beTrue
      constructStreetAddressFrom(l, sd1, Some(dp)).get must beEqualTo("3c-4d Thoroughfarename")
      val sd2 = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(recordType = Some("descriptionForLLPG"))
      constructStreetAddressFrom(l, sd2, Some(dp)).isDefined must beTrue
      constructStreetAddressFrom(l, sd2, Some(dp)).get must beEqualTo("3c-4d Thoroughfarename")
      val sd3 = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(recordType = Some("streetDescription"))
      constructStreetAddressFrom(l, sd3, Some(dp)).isDefined must beTrue
      constructStreetAddressFrom(l, sd3, Some(dp)).get must beEqualTo("3c-4d Thoroughfarename")
    }

    "be none if not officially designated or numbered street and no delivery point entry" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = Some("c"),
        paoEndNumber = Some("4"),
        paoEndSuffix = Some("d"),
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )
      val dp = deliveryPoint("uprn")
      val sd1 = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(recordType = Some("unofficialStreetDescription"))
      constructStreetAddressFrom(l, sd1, None).isDefined must beFalse
      val sd2 = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(recordType = Some("descriptionForLLPG"))
      constructStreetAddressFrom(l, sd2, None).isDefined must beFalse
      val sd3 = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(recordType = Some("streetDescription"))
      constructStreetAddressFrom(l, sd3, None).isDefined must beFalse
    }


    "include pao number/suffix if included" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = Some("c"),
        paoEndNumber = Some("4"),
        paoEndSuffix = Some("d"),
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )

      val sd = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(streetDescription = "Some street")
      constructStreetAddressFrom(l, sd, None).get must beEqualTo("3c-4d Some Street")
    }

    "include street description in sentence case" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = Some("c"),
        paoEndNumber = Some("4"),
        paoEndSuffix = Some("d"),
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )

      val sd = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(streetDescription = "SOME STREET")
      constructStreetAddressFrom(l, sd, None).get must beEqualTo("3c-4d Some Street")
    }

    "include only include pao numbers if no suffixes" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = None,
        paoEndNumber = Some("4"),
        paoEndSuffix = None,
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )

      val sd = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(streetDescription = "SOME STREET")
      constructStreetAddressFrom(l, sd, None).get must beEqualTo("3-4 Some Street")
    }

    "include only include start pao numbers if no end" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = None,
        paoEndNumber = None,
        paoEndSuffix = None,
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )

      val sd = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(streetDescription = "SOME STREET")
      constructStreetAddressFrom(l, sd, None).get must beEqualTo("3 Some Street")
    }

    "include only include end pao numbers if no start" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = None,
        paoStartSuffix = None,
        paoEndNumber = Some("4"),
        paoEndSuffix = None,
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )

      val sd = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(streetDescription = "SOME STREET")
      constructStreetAddressFrom(l, sd, None).get must beEqualTo("4 Some Street")
    }

    "include not include pao suffixes if no numbers" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = None,
        paoStartSuffix = None,
        paoEndNumber = None,
        paoEndSuffix = None,
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )

      val sd = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(streetDescription = "SOME STREET")
      constructStreetAddressFrom(l, sd, None).get must beEqualTo("Some Street")
    }
  }

  "constructPropertyFrom" should {
    "should be correctly created a property from an LPI" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = Some("3"),
        paoEndNumber = Some("c"),
        paoEndSuffix = Some("d"),
        saoText = Some("sao text"),
        paoText = Some("pao text")
      )

      val property = constructPropertyFrom(l)
      property.get must beEqualTo("Sao Text 1a-2b Pao Text")
    }

    "should be correctly return None a property from an LPI with no Sao and Pao details" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = None,
        saoStartSuffix = None,
        saoEndNumber = None,
        saoEndSuffix = None,
        paoStartNumber = None,
        paoStartSuffix = None,
        paoEndNumber = None,
        paoEndSuffix = None,
        saoText = None,
        paoText = None
      )

      val property = constructPropertyFrom(l)
      property.isDefined must beFalse
    }

    "should be correctly created a property from an LPI - with no PAO text" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = Some("3"),
        paoEndNumber = Some("c"),
        paoEndSuffix = Some("d"),
        saoText = Some("sao text"),
        paoText = None
      )

      val property = constructPropertyFrom(l)
      property.get must beEqualTo("Sao Text 1a-2b")
    }

    "should be correctly created a property from an LPI - with no SAO text" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = Some("3"),
        paoEndNumber = Some("c"),
        paoEndSuffix = Some("d"),
        saoText = None,
        paoText = Some("pao text")
      )

      val property = constructPropertyFrom(l)
      property.get must beEqualTo("1a-2b Pao Text")
    }

    "should be correctly created a property from an LPI - with no PAO or SAO text" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = Some("3"),
        paoEndNumber = Some("c"),
        paoEndSuffix = Some("d"),
        saoText = None,
        paoText = None
      )

      val property = constructPropertyFrom(l)
      property.get must beEqualTo("1a-2b")
    }

    "should be correctly created a property from an LPI - with no SAO suffix" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = None,
        saoEndNumber = Some("2"),
        saoEndSuffix = None,
        paoStartNumber = Some("3"),
        paoStartSuffix = Some("c"),
        paoEndNumber = Some("4"),
        paoEndSuffix = Some("d"),
        saoText = None,
        paoText = None
      )

      val property = constructPropertyFrom(l)
      property.get must beEqualTo("1-2")
    }

    "should be correctly created a property from an LPI - with no SAO end" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = None,
        saoEndSuffix = None,
        paoStartNumber = Some("3"),
        paoStartSuffix = Some("c"),
        paoEndNumber = Some("4"),
        paoEndSuffix = Some("d"),
        saoText = Some("sao text"),
        paoText = None
      )

      val property = constructPropertyFrom(l)
      property.get must beEqualTo("Sao Text 1a")

    }
  }

  "details" should {
    "contain all relevant data from LPI, Organisation and Classification objects" in {
      val l = lpi("uprn", "usrn")
      val b = blpu("uprn").copy(receivesPost = "Y")
      val c = classification("uprn").copy(classificationCode = "RD04", primaryUse = "Residential", secondaryUse = Some("Dwelling"))
      val o = organisation("uprn").copy(organistation = "THE ORG")

      val wrapper = AddressBaseWrapper(b, l, c, Some(o), None)
      val d = details(wrapper, "filename")
      d.file must beEqualTo("filename")
      d.blpuCreatedAt must beEqualTo(b.startDate)
      d.blpuUpdatedAt must beEqualTo(b.lastUpdated)
      d.classification must beEqualTo(c.classificationCode)
      d.status.get must beEqualTo(b.blpuState.get.toString)
      d.state.get must beEqualTo(b.logicalState.get.toString)
      d.isPostalAddress must beTrue
      d.isResidential must beTrue
      d.isCommercial must beFalse
      d.isHigherEducational must beFalse
      d.isElectoral must beTrue
      d.usrn must beEqualTo("usrn")
      d.organisation.get must beEqualTo("The Org")
      d.primaryClassification must beEqualTo("Residential")
      d.secondaryClassification.get must beEqualTo("Dwelling")

    }

    "contain all relevant data from LPI, Organisation and Classification objects for educational property" in {
      val l = lpi("uprn", "usrn")
      val b = blpu("uprn").copy(receivesPost = "Y")
      val c = classification("uprn").copy(classificationCode = "CE01", primaryUse = "Education", secondaryUse = Some("College"))
      val o = organisation("uprn").copy(organistation = "THE ORG")

      val wrapper = AddressBaseWrapper(b, l, c, Some(o), None)
      val d = details(wrapper, "filename")
      d.file must beEqualTo("filename")
      d.blpuCreatedAt must beEqualTo(b.startDate)
      d.blpuUpdatedAt must beEqualTo(b.lastUpdated)
      d.classification must beEqualTo(c.classificationCode)
      d.status.get must beEqualTo(b.blpuState.get.toString)
      d.state.get must beEqualTo(b.logicalState.get.toString)
      d.isPostalAddress must beTrue
      d.isResidential must beFalse
      d.isCommercial must beFalse
      d.isHigherEducational must beTrue
      d.isElectoral must beTrue
      d.usrn must beEqualTo("usrn")
      d.organisation.get must beEqualTo("The Org")
      d.primaryClassification must beEqualTo("Education")
      d.secondaryClassification.get must beEqualTo("College")

    }

    "contain all set electoral field to false if not a postal address" in {
      val l = lpi("uprn", "usrn")
      val b = blpu("uprn").copy(receivesPost = "N")
      val c = classification("uprn").copy(classificationCode = "CE01", primaryUse = "Education", secondaryUse = Some("College"))
      val o = organisation("uprn").copy(organistation = "THE ORG")

      val wrapper = AddressBaseWrapper(b, l, c, Some(o), None)
      val d = details(wrapper, "filename")
      d.file must beEqualTo("filename")
      d.blpuCreatedAt must beEqualTo(b.startDate)
      d.blpuUpdatedAt must beEqualTo(b.lastUpdated)
      d.classification must beEqualTo(c.classificationCode)
      d.status.get must beEqualTo(b.blpuState.get.toString)
      d.state.get must beEqualTo(b.logicalState.get.toString)
      d.isPostalAddress must beFalse
      d.isResidential must beFalse
      d.isCommercial must beFalse
      d.isHigherEducational must beTrue
      d.isElectoral must beFalse
      d.usrn must beEqualTo("usrn")
      d.organisation.get must beEqualTo("The Org")
      d.primaryClassification must beEqualTo("Education")
      d.secondaryClassification.get must beEqualTo("College")

    }

    "contain None where optional data from LPI, Organisation and Classification objects is missing" in {
      val l = lpi("uprn", "usrn")
      val b = blpu("uprn").copy(receivesPost = "Y", blpuState = None, logicalState = None)
      val c = classification("uprn").copy(classificationCode = "RD04", primaryUse = "Residential", secondaryUse = None)

      val wrapper = AddressBaseWrapper(b, l, c, None, None)
      val d = details(wrapper, "filename")
      d.file must beEqualTo("filename")
      d.classification must beEqualTo(c.classificationCode)
      d.status.isDefined must beFalse
      d.state.isDefined must beFalse
      d.isPostalAddress must beTrue
      d.isResidential must beTrue
      d.isCommercial must beFalse
      d.isHigherEducational must beFalse
      d.usrn must beEqualTo("usrn")
      d.organisation.isDefined must beFalse
      d.primaryClassification must beEqualTo("Residential")
      d.secondaryClassification.isDefined must beFalse
    }

    "contain correctly set postal vote field to be true" in {
      val l = lpi("uprn", "usrn")
      val b = blpu("uprn").copy(receivesPost = "Y")
      val c = classification("uprn")

      val wrapper = AddressBaseWrapper(b, l, c, None, None)
      val d = details(wrapper, "filename")
      d.isPostalAddress must beTrue

    }

    "contain correctly set postal vote field to be false" in {
      val l = lpi("uprn", "usrn")
      val b = blpu("uprn").copy(receivesPost = "N")
      val c = classification("uprn")

      val wrapper = AddressBaseWrapper(b, l, c, None, None)
      val d = details(wrapper, "filename")
      d.isPostalAddress must beFalse

    }
  }

  // TODO TEST ME
  "presentation" should {
    "set up complete set of values, all in sentence case" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = Some("c"),
        paoEndNumber = Some("4"),
        paoEndSuffix = Some("d"),
        saoText = Some("sao text"),
        paoText = Some("pao text")
      )
      val b = blpu("uprn").copy(receivesPost = "Y", postcode = "MY1 1MY")
      val sd = StreetDescriptor("usrn", "high street", Some("locality"), Some("my town"), "admin area")
      val s = streetWithDescription("filename", sd, street("usrn"))

      val p = presentation(b, l, s, None)
      p.property.get must beEqualTo("Sao Text 1a-2b Pao Text")
      p.street.get must beEqualTo("3c-4d High Street")
      p.locality.get must beEqualTo("Locality")
      p.town.get must beEqualTo("My Town")
      p.postcode must beEqualTo("MY1 1MY")
      p.area.get must beEqualTo("Admin Area")
    }

    "set up complete set of values, all in sentence case, street derived from delivery point" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoStartSuffix = Some("a"),
        saoEndNumber = Some("2"),
        saoEndSuffix = Some("b"),
        paoStartNumber = Some("3"),
        paoStartSuffix = Some("c"),
        paoEndNumber = Some("4"),
        paoEndSuffix = Some("d"),
        saoText = Some("sao text"),
        paoText = Some("pao text")
      )
      val b = blpu("uprn").copy(receivesPost = "Y", postcode = "MY1 1MY")
      val sd = StreetDescriptor("usrn", "high street", Some("locality"), Some("my town"), "admin area")
      val s = streetWithDescription("filename", sd, street("usrn")).copy(recordType = Some("unofficialStreetDescription"))
      val dp = deliveryPoint("uprn").copy(buildingNumber = Some("123"), thoroughfareName = Some("street"))

      val p = presentation(b, l, s, Some(dp))
      p.property.get must beEqualTo("Sao Text 1a-2b Pao Text")
      p.street.get must beEqualTo("3c-4d Street")
      p.locality.get must beEqualTo("Locality")
      p.town.get must beEqualTo("My Town")
      p.postcode must beEqualTo("MY1 1MY")
      p.area.get must beEqualTo("Admin Area")
    }

    "set up minimum set of values, all in sentence case" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = None,
        saoStartSuffix = None,
        saoEndNumber = None,
        saoEndSuffix = None,
        paoStartNumber = None,
        paoStartSuffix = None,
        paoEndNumber = None,
        paoEndSuffix = None,
        saoText = None,
        paoText = None
      )

      val b = blpu("uprn").copy(receivesPost = "Y", postcode = "MY1 1MY")
      val sd = StreetDescriptor("usrn", "high street", None, None, "admin area")
      val s = streetWithDescription("filename", sd, street("usrn"))

      val p = presentation(b, l, s, None)
      p.property.isDefined must beFalse
      p.street.get must beEqualTo("High Street")
      p.locality.isDefined must beFalse
      p.town.isDefined must beFalse
      p.postcode must beEqualTo("MY1 1MY")
      p.area.get must beEqualTo("Admin Area")
    }


    "should set area to none if area and town are the same" in {
      val l = lpi("uprn", "usrn")
      val b = blpu("uprn").copy(receivesPost = "Y", postcode = "MY1 1MY")
      val sd = StreetDescriptor("usrn", "high street", None, Some("thing"), "thing")
      val s = streetWithDescription("filename", sd, street("usrn"))

      val p = presentation(b, l, s, None)
      p.town.isDefined must beTrue
      p.town.get must beEqualTo("Thing")
      p.area.isDefined must beFalse
    }
  }

  "this" should {
    "fail if no street and no property found?" in {
      1 must beEqualTo(2)
    }
  }
}
