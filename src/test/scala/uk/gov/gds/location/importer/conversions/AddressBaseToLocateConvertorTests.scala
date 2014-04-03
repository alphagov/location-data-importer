package uk.gov.gds.location.importer.conversions

import org.specs2.mutable.Specification
import org.specs2.mock.Mockito
import uk.gov.gds.location.importer.model.{StreetWithDescription, AllTheCodePoints, AllTheStreets, AddressBaseWrapper}
import uk.gov.gds.location.importer.helpers.TestHelpers._

class AddressBaseToLocateConvertorTests extends Specification with Mockito {

  import AddressBaseToLocateConvertor._
  import formatters._

  sequential

  "Address conversions" should {

    "not create an address if no street available for the usrn" in {
      val addressWrapper = AddressBaseWrapper(blpu("blpu"), lpi("uprn", "usrn"), classification("uprn"), None)
      toLocateAddress(addressWrapper, "filename") must beEqualTo(None)
    }

    "not create an address if street available for the usrn but no codepoint for postcode" in {
      AllTheStreets.add(List(streetWithDescription("filename", streetDescriptor("usrn"), street("usrn"))))
      val addressWrapper = AddressBaseWrapper(blpu("blpu"), lpi("uprn", "usrn"), classification("uprn"), None)
      toLocateAddress(addressWrapper, "filename") must beEqualTo(None)
    }

    "create an address if street and codepoint available for the address" in {
      AllTheStreets.add(List(streetWithDescription("filename", streetDescriptor("usrn"), street("usrn"))))
      AllTheCodePoints.add(List(codePoint))
      val addressWrapper = AddressBaseWrapper(blpu("blpu"), lpi("uprn", "usrn"), classification("uprn"), None)
      toLocateAddress(addressWrapper, "filename").isDefined must beTrue
    }
  }

  "Orderings" should {
    "build full set of ordering information from sao and pao fields in LPI" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = Some("1"),
        saoEndNumber = Some("2"),
        paoStartNumber = Some("3"),
        paoEndNumber = Some("4"),
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )
      val o = ordering(AddressBaseWrapper(blpu("uprn"), l, classification("uprn"), None))
      o.saoStartNumber.get must beEqualTo(1)
      o.saoEndNumber.get must beEqualTo(2)
      o.paoStartNumber.get must beEqualTo(3)
      o.paoEndNumber.get must beEqualTo(4)
      o.saoText.get must beEqualTo("saotext")
      o.paoText.get must beEqualTo("paotext")
    }

    "build empty set of ordering information from empty sao and pao fields in LPI" in {
      val l = lpi("uprn", "usrn").copy(
        saoStartNumber = None,
        saoEndNumber = None,
        paoStartNumber = None,
        paoEndNumber = None,
        saoText = None,
        paoText = None
      )
      val o = ordering(AddressBaseWrapper(blpu("uprn"), l, classification("uprn"), None))
      o.saoStartNumber must beEqualTo(None)
      o.saoEndNumber must beEqualTo(None)
      o.paoStartNumber must beEqualTo(None)
      o.paoEndNumber must beEqualTo(None)
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
      val o = ordering(AddressBaseWrapper(blpu("uprn"), l, classification("uprn"), None))
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
      val o = ordering(AddressBaseWrapper(blpu("uprn"), l, classification("uprn"), None))
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



  "Presentation" should {
    "correctly create the property field" in {
      1 must beEqualTo(2)
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
    "be None if not officially designated street" in {
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

      val sd = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(recordType = Some("streetDescription"))
      constructStreetAddressFrom(l, sd).isDefined must beFalse
    }

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

      val sd1 = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(recordType = Some("officiallyDesignated"))
      constructStreetAddressFrom(l, sd1).isDefined must beTrue
      val sd2 = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(recordType = Some("numberedStreet"))
      constructStreetAddressFrom(l, sd2).isDefined must beTrue
    }

    "be not populated if not officially designated or numbered street" in {
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

      val sd1 = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(recordType = Some("unofficialStreetDescription"))
      constructStreetAddressFrom(l, sd1).isDefined must beFalse
      val sd2 = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(recordType = Some("descriptionForLLPG"))
      constructStreetAddressFrom(l, sd2).isDefined must beFalse
      val sd3 = streetWithDescription("filename", streetDescriptor("usrn"), street("usrn")).copy(recordType = Some("streetDescription"))
      constructStreetAddressFrom(l, sd3).isDefined must beFalse
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
      constructStreetAddressFrom(l, sd).get must beEqualTo("3c-4d Some Street")
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
      constructStreetAddressFrom(l, sd).get must beEqualTo("3c-4d Some Street")
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
      constructStreetAddressFrom(l, sd).get must beEqualTo("3-4 Some Street")
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
      constructStreetAddressFrom(l, sd).get must beEqualTo("3 Some Street")
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
      constructStreetAddressFrom(l, sd).get must beEqualTo("4 Some Street")
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
      constructStreetAddressFrom(l, sd).get must beEqualTo("Some Street")
    }
  }

  "constructPropertyFrom" should {
    "do something" in {
      1 must beEqualTo(2) // do this
    }
  }

  "Property fields" should {
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
        saoText = Some("saotext"),
        paoText = Some("paotext")
      )

      val property = constructPropertyFrom(l)
      property must beEqualTo("SaoText 1a-2b PaoText")
    }
  }

  //
  //    "include the postcode from the BLPU all lowercased on the root address object" in {
  //      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu.copy(postcode = "SE45 0PP"), lpi, classification, Some(organisation)))(Some(mongoConnection), randomFilename).get.postcode must beEqualTo("se450pp")
  //    }
  //
  //    "include the gss code and country code from the codepoint table on the root address object" in {
  //      val mongoConnectionWithSpecificCodePoint = mock[MongoConnection]
  //      mongoConnectionWithSpecificCodePoint.streetForUsrn(anyString) returns Some(streetWithDescription)
  //      mongoConnectionWithSpecificCodePoint.codePointForPostcode(validBlpu.postcode) returns Some(CodePoint("postcode", "specific country", "countycode", "specific district", "wardcode"))
  //      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu, lpi, classification, Some(organisation)))(Some(mongoConnectionWithSpecificCodePoint), randomFilename).get.gssCode must beEqualTo("specific district")
  //      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu, lpi, classification, Some(organisation)))(Some(mongoConnectionWithSpecificCodePoint), randomFilename).get.countryCode must beEqualTo("specific country")
  //    }
  //
  //    "return no address if no code point available for the postcode" in {
  //      val mongoConnectionWithNoCodePoint = mock[MongoConnection]
  //      mongoConnectionWithNoCodePoint.streetForUsrn(anyString) returns Some(streetWithDescription)
  //      mongoConnectionWithNoCodePoint.codePointForPostcode(anyString) returns None
  //      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu, lpi, classification, Some(organisation)))(Some(mongoConnectionWithNoCodePoint), randomFilename) must beEqualTo(None)
  //    }
  //
  //    "include the pao text from the LPI on the root address object as houseName" in {
  //      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu, lpi.copy(paoText = "house name"), classification, Some(organisation)))(Some(mongoConnection), randomFilename).get.houseName.get must beEqualTo("House Name")
  //    }
  //
  //    "not include house name if the pao text from the LPI is absent" in {
  //      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu, lpi.copy(paoText = None), classification, Some(organisation)))(Some(mongoConnection), randomFilename).get.houseName must beEqualTo(None)
  //    }
  //
  //    "include house number generated from the LPI PAO fields on the root address object" in {
  //      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu,
  //        lpi.copy(paoStartNumber = Some("99"), paoStartSuffix = Some("A"), paoEndNumber = Some("100"), paoEndSuffix = Some("Z"), saoStartNumber = Some("9"), saoStartSuffix = Some("A"), saoEndNumber = Some("10"), saoEndSuffix = Some("B"))
  //        , classification, Some(organisation)))(Some(mongoConnection), randomFilename).get.houseNumber.get must beEqualTo("99A-100Z")
  //    }
  //
  //    "not include house number if no LPI PAO fields - ignoring SAO fields" in {
  //      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu,
  //        lpi.copy(paoStartNumber = None, paoStartSuffix = None, paoEndNumber = None, paoEndSuffix = None, saoStartNumber = Some("9"), saoStartSuffix = Some("A"), saoEndNumber = Some("10"), saoEndSuffix = Some("B"))
  //        , classification, Some(organisation)))(Some(mongoConnection), randomFilename).get.houseNumber must beEqualTo(None)
  //    }
  //
  //    "include the postcode from the BLPU as supplied on the presentation address object" in {
  //      geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu.copy(postcode = "SE45 0PP"), lpi, classification, Some(organisation)))(Some(mongoConnection), randomFilename).get.presentation.postcode must beEqualTo("SE45 0PP")
  //    }
  //
  //    "construct suffixes - if start and end they should be seperated by a dash is both present" in {
  //      formatStartAndEndNumbersAndSuffixes(Some("10"), Some("A"), Some("11"), Some("B")).get must beEqualTo("10A-11B")
  //    }
  //
  //    "construct suffixes - if start and end they should be seperated by a dash is both present - with no suffixes" in {
  //      formatStartAndEndNumbersAndSuffixes(Some("10"), None, Some("11"), None).get must beEqualTo("10-11")
  //    }
  //
  //    "construct suffixes - if start and end they should be seperated by a dash is both present - with only start suffix" in {
  //      formatStartAndEndNumbersAndSuffixes(Some("10"), Some("A"), Some("11"), None).get must beEqualTo("10A-11")
  //    }
  //
  //    "construct suffixes - if start and end they should be seperated by a dash is both present - with only end suffix" in {
  //      formatStartAndEndNumbersAndSuffixes(Some("10"), None, Some("11"), Some("B")).get must beEqualTo("10-11B")
  //    }
  //
  //    "construct suffixes - start only with suffix" in {
  //      formatStartAndEndNumbersAndSuffixes(Some("10"), Some("A"), None, None).get must beEqualTo("10A")
  //    }
  //
  //    "construct suffixes - start only with no suffix" in {
  //      formatStartAndEndNumbersAndSuffixes(Some("10"), None, None, None).get must beEqualTo("10")
  //    }
  //
  //    "construct suffixes - start only with suffix ignores end suffix if no end number" in {
  //      formatStartAndEndNumbersAndSuffixes(Some("10"), Some("A"), None, Some("B")).get must beEqualTo("10A")
  //    }
  //
  //    "construct suffixes - should be None if no suffixes / numbers present" in {
  //      formatStartAndEndNumbersAndSuffixes(None, None, None, None) must beEqualTo(None)
  //    }
  //
  //    "construct suffixes - should be None if only end suffixes and numbers present" in {
  //      formatStartAndEndNumbersAndSuffixes(None, None, Some("10"), Some("B")) must beEqualTo(None)
  //    }
  //
  //    "construct suffixes - should be None if only end suffixes present" in {
  //      formatStartAndEndNumbersAndSuffixes(None, None, None, Some("B")) must beEqualTo(None)
  //    }
  //
  //    "construct suffixes - should be None if only end numbers present" in {
  //      formatStartAndEndNumbersAndSuffixes(None, None, Some("10"), None) must beEqualTo(None)
  //    }
  //
  //    "construct suffixes - should be None if only start suffix present" in {
  //      formatStartAndEndNumbersAndSuffixes(None, Some("B"), None, None) must beEqualTo(None)
  //    }
  //
  //    "should transform all the SAO fields plus PAO Text into a string excluding the pao fields fornumber and suffix" in {
  //      constructPropertyFrom(lpi).get must beEqualTo("sao start numbersao start suffix-sao end numbersao end suffix sao text pao text")
  //    }
  //
  //    "should transform all the SAO fields supplied plus PAO Text into a string" in {
  //      constructPropertyFrom(lpiWithSaoStartAndPaoTextAndNoSaoEndOrSaoText).get must beEqualTo("sao start numbersao start suffix pao text")
  //    }
  //
  //    "should transform all the SAO and PAO fields successfully when no suffix" in {
  //      constructPropertyFrom(lpiWithNoSuffix).get must beEqualTo("sao start number-sao end number sao text pao text")
  //    }
  //
  //    "should transform all the SAO and PAO fields excluding the suffixs when no numbers" in {
  //      constructPropertyFrom(lpiWithNoNumbers).get must beEqualTo("sao text pao text")
  //    }
  //
  //    "should transform all missing sao and pao text fields into an empty string that returns true to isEmpty" in {
  //      constructPropertyFrom(lpiWithNoSaoFieldsAndNoPaoText) must beEqualTo(None)
  //    }
  //
  //    "should format a street address from the pao start and end numbers on an LPI" in {
  //      constructStreetAddressPrefixFrom(lpi).get must beEqualTo("pao start numberpao start suffix-pao end numberpao end suffix")
  //    }
  //
  //    "should return None for a street address if no pao start number on an LPI" in {
  //      constructStreetAddressPrefixFrom(lpi.copy(paoStartNumber = None)) must beEqualTo(None)
  //    }
  //
  //    "should construct a location object from the x and y of a blpu" in {
  //      location(validBlpu).x must beEqualTo(1.1)
  //      location(validBlpu).y must beEqualTo(2.2)
  //    }
  //
  //    "should construct a street address from the LPI and the street description" in {
  //      constructStreetAddressFrom(
  //        lpi.copy(paoStartNumber = Some("10"), paoStartSuffix = Some("a"), paoEndNumber = Some("11"), paoEndSuffix = Some("b")), streetWithDescription.copy(streetDescription = "The Street")
  //      ).get must beEqualTo("10a-11b The Street")
  //    }
  //
  //    "should construct a street address from the LPI with no pao start suffix and the street description" in {
  //      constructStreetAddressFrom(
  //        lpi.copy(paoStartNumber = Some("10"), paoStartSuffix = None, paoEndNumber = Some("11"), paoEndSuffix = Some("b")), streetWithDescription.copy(streetDescription = "The Street")
  //      ).get must beEqualTo("10-11b The Street")
  //    }
  //
  //    "should construct a street address from the LPI with no pao end and the street description" in {
  //      constructStreetAddressFrom(
  //        lpi.copy(paoStartNumber = Some("10"), paoStartSuffix = None, paoEndNumber = None, paoEndSuffix = None), streetWithDescription.copy(streetDescription = "The Street")
  //      ).get must beEqualTo("10 The Street")
  //    }
  //
  //
  //    "make a details object containing the correct classification information" in {
  //      details(AddressBaseWrapper(validBlpu, lpi, classification.copy(classificationCode = "R1"), None), "filename").classification must beEqualTo("R1")
  //      details(AddressBaseWrapper(validBlpu, lpi, classification.copy(classificationCode = "R1"), None), "filename").isResidential must beEqualTo(true)
  //      details(AddressBaseWrapper(validBlpu, lpi, classification.copy(classificationCode = "C1"), None), "filename").isCommercial must beEqualTo(true)
  //    }
  //
  //    "make a detail object that correctly interprets the postal address" in {
  //      details(AddressBaseWrapper(validBlpu.copy(receivesPost = "N"), lpi, classification, None), "filename").isPostalAddress must beEqualTo(false)
  //    }
  //
  //    "make a detail object that correctly set the usrn" in {
  //      details(AddressBaseWrapper(validBlpu, lpi.copy(usrn = "USRN"), classification, None), "filename").usrn must beEqualTo("USRN")
  //    }
  //
  //    "make a detail object that correctly contains the updated and created dates from the BLPU" in {
  //      details(AddressBaseWrapper(validBlpu.copy(startDate = startDate), lpi, classification, None), "filename").blpuCreatedAt must beEqualTo(startDate)
  //      details(AddressBaseWrapper(validBlpu.copy(lastUpdated = lastUpdatedDate), lpi, classification, None), "filename").blpuUpdatedAt must beEqualTo(lastUpdatedDate)
  //    }
  //
  //    "make a detail object that correctly contains the BLPU logical status and state codes" in {
  //      details(AddressBaseWrapper(validBlpu.copy(logicalState = Some(LogicalStatusCode.approved)), lpi, classification, None), "filename").state.get must beEqualTo("approved")
  //      details(AddressBaseWrapper(validBlpu.copy(blpuState = Some(BlpuStateCode.inUse)), lpi, classification, None), "filename").status.get must beEqualTo("inUse")
  //    }
  //
  //    "make an address object with all the fields set up correctly" in {
  //      val filename = randomFilename
  //      val address = geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu, lpi, classification, Some(organisation)))(Some(mongoConnection), filename).get
  //
  //      /* base object */
  //      address.gssCode must beEqualTo("districtcode")
  //      address.postcode must beEqualTo("postcode")
  //      address.houseName.get must beEqualTo("Pao Text")
  //      address.houseNumber.get must beEqualTo("pao start numberpao start suffix-pao end numberpao end suffix")
  //
  //      /* details */
  //      address.details.blpuCreatedAt must beEqualTo(startDate)
  //      address.details.blpuUpdatedAt must beEqualTo(lastUpdatedDate)
  //      address.details.classification must beEqualTo("code")
  //      address.details.isPostalAddress must beEqualTo(true)
  //      address.details.isResidential must beEqualTo(false)
  //      address.details.usrn must beEqualTo("usrn")
  //      address.details.state.get must beEqualTo("approved")
  //      address.details.status.get must beEqualTo("inUse")
  //      address.details.file must beEqualTo(filename)
  //      address.details.organisation.get must beEqualTo("Organisation")
  //
  //      /* location */
  //      address.location.x must beEqualTo(1.1)
  //      address.location.y must beEqualTo(2.2)
  //
  //      /* presentation */
  //      address.presentation.postcode must beEqualTo("postcode")
  //      address.presentation.uprn must beEqualTo("uprn")
  //      address.presentation.town.get must beEqualTo("Town")
  //      address.presentation.area.get must beEqualTo("Area")
  //      address.presentation.locality.get must beEqualTo("Locality")
  //      address.presentation.street.get must beEqualTo("Pao Start Numberpao Start Suffix-pao End Numberpao End Suffix Street Name")
  //      address.presentation.property.get must beEqualTo("Sao Start Numbersao Start Suffix-sao End Numbersao End Suffix Sao Text Pao Text")
  //    }
  //
  //    "make an address object with no area name if area name and town are identical" in {
  //      val filename = randomFilename
  //      val mongoConnectionWithTownAndAreaTheSame = mock[MongoConnection]
  //      mongoConnectionWithTownAndAreaTheSame.codePointForPostcode(validBlpu.postcode) returns Some(codePoint)
  //
  //      mongoConnectionWithTownAndAreaTheSame.streetForUsrn(anyString) returns Some(streetWithDescription.copy(townName = Some("something"), administrativeArea = "something"))
  //
  //      val address = geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu, lpi, classification, Some(organisation)))(Some(mongoConnectionWithTownAndAreaTheSame), filename).get
  //
  //      /* presentation */
  //      address.presentation.postcode must beEqualTo("postcode")
  //      address.presentation.uprn must beEqualTo("uprn")
  //      address.presentation.town.get must beEqualTo("Something")
  //      address.presentation.area must beEqualTo(None)
  //      address.presentation.locality.get must beEqualTo("Locality")
  //      address.presentation.street.get must beEqualTo("Pao Start Numberpao Start Suffix-pao End Numberpao End Suffix Street Name")
  //      address.presentation.property.get must beEqualTo("Sao Start Numbersao Start Suffix-sao End Numbersao End Suffix Sao Text Pao Text")
  //    }
  //
  //
  //    "make an address object with no street description if street name is not official" in {
  //      val mongoConnectionWithNoStreet = mock[MongoConnection]
  //      mongoConnectionWithNoStreet.streetForUsrn(anyString) returns Some(streetWithDescription.copy(recordType = Some("unoffical")))
  //      mongoConnectionWithNoStreet.codePointForPostcode(anyString) returns Some(codePoint)
  //      val address = geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu, lpi, classification, None))(Some(mongoConnectionWithNoStreet), randomFilename).get
  //
  //      /* base object */
  //      address.gssCode must beEqualTo("districtcode")
  //      address.postcode must beEqualTo("postcode")
  //      address.houseName.get must beEqualTo("Pao Text")
  //      address.houseNumber.get must beEqualTo("pao start numberpao start suffix-pao end numberpao end suffix")
  //
  //      /* details */
  //      address.details.blpuCreatedAt must beEqualTo(startDate)
  //      address.details.blpuUpdatedAt must beEqualTo(lastUpdatedDate)
  //      address.details.classification must beEqualTo("code")
  //      address.details.isPostalAddress must beEqualTo(true)
  //      address.details.isResidential must beEqualTo(false)
  //      address.details.usrn must beEqualTo("usrn")
  //      address.details.state.get must beEqualTo("approved")
  //      address.details.status.get must beEqualTo("inUse")
  //      address.details.organisation must beEqualTo(None)
  //
  //      /* location */
  //      address.location.x must beEqualTo(1.1)
  //      address.location.y must beEqualTo(2.2)
  //
  //      /* presentation */
  //      address.presentation.postcode must beEqualTo("postcode")
  //      address.presentation.uprn must beEqualTo("uprn")
  //      address.presentation.town.get must beEqualTo("Town")
  //      address.presentation.area.get must beEqualTo("Area")
  //      address.presentation.locality.get must beEqualTo("Locality")
  //      address.presentation.street must beEqualTo(None)
  //      address.presentation.property.get must beEqualTo("Sao Start Numbersao Start Suffix-sao End Numbersao End Suffix Sao Text Pao Text")
  //    }
  //
  //
  //    "make an address object with  no organistaion if none set" in {
  //      val address = geographicAddressToSimpleAddress(AddressBaseWrapper(validBlpu, lpi, classification, None))(Some(mongoConnection), randomFilename).get
  //
  //      /* base object */
  //      address.gssCode must beEqualTo("districtcode")
  //      address.postcode must beEqualTo("postcode")
  //      address.houseName.get must beEqualTo("Pao Text")
  //      address.houseNumber.get must beEqualTo("pao start numberpao start suffix-pao end numberpao end suffix")
  //
  //      /* details */
  //      address.details.blpuCreatedAt must beEqualTo(startDate)
  //      address.details.blpuUpdatedAt must beEqualTo(lastUpdatedDate)
  //      address.details.classification must beEqualTo("code")
  //      address.details.isPostalAddress must beEqualTo(true)
  //      address.details.isResidential must beEqualTo(false)
  //      address.details.usrn must beEqualTo("usrn")
  //      address.details.state.get must beEqualTo("approved")
  //      address.details.status.get must beEqualTo("inUse")
  //      address.details.organisation must beEqualTo(None)
  //
  //      /* location */
  //      address.location.x must beEqualTo(1.1)
  //      address.location.y must beEqualTo(2.2)
  //
  //      /* presentation */
  //      address.presentation.postcode must beEqualTo("postcode")
  //      address.presentation.uprn must beEqualTo("uprn")
  //      address.presentation.town.get must beEqualTo("Town")
  //      address.presentation.area.get must beEqualTo("Area")
  //      address.presentation.locality.get must beEqualTo("Locality")
  //      address.presentation.street must beEqualTo(Some("Pao Start Numberpao Start Suffix-pao End Numberpao End Suffix Street Name"))
  //      address.presentation.property.get must beEqualTo("Sao Start Numbersao Start Suffix-sao End Numbersao End Suffix Sao Text Pao Text")
  //    }
  //
  //    "convert strings to sentence case" in {
  //      toSentenceCase("THIS IS NOT IN SENTENCE CASE").get must beEqualTo("This Is Not In Sentence Case")
  //      toSentenceCase("THIS ÎS NÔT IN SENTENCE CASE").get must beEqualTo("This Îs Nôt In Sentence Case")
  //      toSentenceCase("123 THIS IS NOT IN SENTENCE CASE").get must beEqualTo("123 This Is Not In Sentence Case")
  //      toSentenceCase("123 THIS, IS, NOT, IN, SENTENCE CASE").get must beEqualTo("123 This, Is, Not, In, Sentence Case")
  //    }
  //  }
  //
  //  private lazy val startDate = new DateTime().minusDays(100)
  //  private lazy val lastUpdatedDate = new DateTime().minusDays(50)
  //
  //  private lazy val validBlpu = BLPU(
  //    "uprn",
  //    Some(BlpuStateCode.inUse),
  //    Some(LogicalStatusCode.approved),
  //    1.1,
  //    2.2,
  //    "1234",
  //    startDate,
  //    None,
  //    lastUpdatedDate,
  //    "S",
  //    "postcode")
  //
  //  private lazy val invalidBlpuDueToPostalStatus = validBlpu.copy(receivesPost = "N")
  //  private lazy val invalidBlpuDueToLogicalStatus = validBlpu.copy(logicalState = Some(LogicalStatusCode.historical))
  //  private lazy val invalidBlpuDueToBlpuStatus = validBlpu.copy(blpuState = Some(BlpuStateCode.noLongerExists))
  //  private lazy val invalidBlpuDueToEndDates = validBlpu.copy(endDate = Some(new DateTime()))
  //  private lazy val blpuProvisonal = validBlpu.copy(logicalState = Some(LogicalStatusCode.provisional))
  //
  //  private lazy val lpi = LPI(
  //    "uprn",
  //    "usrn",
  //    Some(LogicalStatusCode.approved),
  //    startDate,
  //    None,
  //    lastUpdatedDate,
  //    Some("pao start number"),
  //    Some("pao start suffix"),
  //    Some("pao end number"),
  //    Some("pao end suffix"),
  //    Some("pao text"),
  //    Some("sao start number"),
  //    Some("sao start suffix"),
  //    Some("sao end number"),
  //    Some("sao end suffix"),
  //    Some("sao text"),
  //    Some("area name"),
  //    Some(true)
  //  )
  //
  //  private lazy val lpiWithNoSuffix = lpi.copy(saoStartSuffix = None, saoEndSuffix = None, paoStartSuffix = None, paoEndSuffix = None)
  //  private lazy val lpiWithNoNumbers = lpi.copy(saoStartNumber = None, saoEndNumber = None, paoStartNumber = None, paoEndNumber = None)
  //
  //  private lazy val lpiWithSaoStartAndPaoTextAndNoSaoEndOrSaoText = lpi.copy(saoStartNumber = Some("sao start number"), saoStartSuffix = Some("sao start suffix"), saoEndNumber = None, saoEndSuffix = None, saoText = None, paoText = Some("pao text"))
  //
  //  private lazy val lpiWithNoSaoFieldsAndNoPaoText = lpi.copy(
  //    saoStartNumber = None, saoStartSuffix = None, saoEndNumber = None, saoEndSuffix = None, saoText = None,
  //    paoStartNumber = None, paoStartSuffix = None, paoEndNumber = None, paoEndSuffix = None, paoText = None
  //  )
  //
  //
  //  private lazy val classification = Classification("uprn", "code", startDate, None, lastUpdatedDate)
  //  private lazy val organisation = Organisation("uprn", "organisation", startDate, None, lastUpdatedDate)
  //  private lazy val streetWithDescription = StreetWithDescription("usrn", "street name", Some("locality"), Some("town"), "area", Some("officiallyDesignated"), Some("state"), Some("code"), Some("classification"), "file")
  //  private lazy val codePoint = CodePoint("postcode", "countrycode", "countycode", "districtcode", "wardcode")

}
