package uk.gov.gds.model

import org.specs2.mutable.Specification
import uk.gov.gds.model.CodeLists._
import org.joda.time.DateTime
import scala.Some

class AddressBuilderTests extends Specification {

  "The address builder" should {
//    "include the postcode from the BLPU" in {
//      AddressBuilder.geographicAddressToSimpleAddress(AddressBaseWrapper(blup, List(lpi), List(classification), List(organisation)), streetMap, streetDescriptorMap).get.postcode must beEqualTo("postcode")
//    }
//
//    "include not create an address if no LPI" in {
//      AddressBuilder.geographicAddressToSimpleAddress(AddressBaseWrapper(blup, List.empty[LPI], List(classification), List(organisation))) must beEqualTo(None)
//    }
//
//    "include not create an address if no eligible BLPU (logical status not approved)" in {
//      AddressBuilder.geographicAddressToSimpleAddress(AddressBaseWrapper(blpuProvisonal, List(lpi), List(classification), List(organisation))) must beEqualTo(None)
//    }
//
//    "include not create an address if no eligible LPI (end date present)" in {
//      AddressBuilder.geographicAddressToSimpleAddress(AddressBaseWrapper(blup, List(lpiWithEndDate), List(classification), List(organisation))) must beEqualTo(None)
//    }
//
//    "include not create an address if no eligible LPI (logical status not approved)" in {
//      AddressBuilder.geographicAddressToSimpleAddress(AddressBaseWrapper(blup, List(lpiProvisional), List(classification), List(organisation))) must beEqualTo(None)
//    }


//    "if there is more than one LPI for a BLPU use the one that has no end date" in {
//      AddressBuilder.geographicAddressToSimpleAddress(AddressBaseWrapper(blup, List(lpi, lpiWithEndDate), List(classification), List(organisation)), streetMap, streetDescriptorMap).get.postcode must beEqualTo("postcode")
//      AddressBuilder.geographicAddressToSimpleAddress(AddressBaseWrapper(blup, List(lpi, lpiWithEndDate), List(classification), List(organisation)), streetMap, streetDescriptorMap).get.line1 must beEqualTo("sao text sao start number sao start suffix sao end number sao end suffix")
//      AddressBuilder.geographicAddressToSimpleAddress(AddressBaseWrapper(blup, List(lpi, lpiWithEndDate), List(classification), List(organisation)), streetMap, streetDescriptorMap).get.line2 must beEqualTo("pao text pao start number pao start suffix pao end number pao end suffix")
//    }
//
//    "if there is more than one LPI for a BLPU and both have no end date use the approved one" in {
//      AddressBuilder.geographicAddressToSimpleAddress(AddressBaseWrapper(blup, List(lpi, lpiProvisional), List(classification), List(organisation)), streetMap, streetDescriptorMap).get.postcode must beEqualTo("postcode")
//      AddressBuilder.geographicAddressToSimpleAddress(AddressBaseWrapper(blup, List(lpi, lpiProvisional), List(classification), List(organisation)), streetMap, streetDescriptorMap).get.line1 must beEqualTo("sao text sao start number sao start suffix sao end number sao end suffix")
//      AddressBuilder.geographicAddressToSimpleAddress(AddressBaseWrapper(blup, List(lpi, lpiProvisional), List(classification), List(organisation)), streetMap, streetDescriptorMap).get.line2 must beEqualTo("pao text pao start number pao start suffix pao end number pao end suffix")
//    }

    "should transform all the SAO fields plus PAO Text into a string" in {
       AddressBuilder.constructProperty(lpi).get must beEqualTo("sao start numbersao start suffix-sao end numbersao end suffix sao text pao text")
    }

    "should transform all the SAO fields supplied plus PAO Text into a string" in {
      AddressBuilder.constructProperty(lpiWithSaoStartAndPaoText).get must beEqualTo("sao start numbersao start suffix pao text")
    }

    "should transform all the SAO and PAO fields successfully when no suffix" in {
      AddressBuilder.constructProperty(lpiWithNoSuffix).get must beEqualTo("sao start number-sao end number sao text pao text")
      AddressBuilder.constructStreetAddress(lpiWithNoSuffix) must beEqualTo("pao start number-pao end number")
    }

    "should transform all the SAO and PAO fields successfully when no numbers" in {
      AddressBuilder.constructProperty(lpiWithNoNumbers).get must beEqualTo("sao text pao text")
      AddressBuilder.constructStreetAddress(lpiWithNoNumbers).isEmpty must beTrue
    }

    "should transform all missing sao and pao text fields into an empty string that returns true to isEmpty" in {
      AddressBuilder.constructProperty(lpiWithNoSaoFieldsAndNoPaoText) must beEqualTo(None)
    }

    "should transform all the PAO fields into a string" in {
      AddressBuilder.constructStreetAddress(lpi) must beEqualTo("pao start numberpao start suffix-pao end numberpao end suffix")
    }

    "valid blpus should be marked as valid" in {
      AddressBuilder.isValidBLPU(blpu) must beTrue
    }

    "invalid blpu due to recieve post must be marked invalid" in {
      AddressBuilder.isValidBLPU(invalidBlpuDueToPostalStatus) must beFalse
    }

    "invalid blpu due to blpu status must be marked invalid" in {
      AddressBuilder.isValidBLPU(invalidBlpuDueToLogicalStatus) must beFalse
    }

    "invalid blpu due to logical state must be marked invalid" in {
      AddressBuilder.isValidBLPU(invalidBlpuDueToBlpuStatus) must beFalse
    }

    "invalid blpu due to end date must be marked invalid" in {
      AddressBuilder.isValidBLPU(invalidBlpuDueToEndDates) must beFalse
    }

  }


  private lazy val startDate = new DateTime().minusDays(100)
  private lazy val endDate = new DateTime().minusDays(1)
  private lazy val lastUpdatedDate = new DateTime().minusDays(50)

  private lazy val blpu = BLPU(
    "uprn",
    Some(BlpuStateCode.inUse),
    Some(LogicalStatusCode.approved),
    1.1,
    2.2,
    1234,
    startDate,
    None,
    lastUpdatedDate,
    "S",
    "postcode")

  private lazy val invalidBlpuDueToPostalStatus = blpu.copy(receivesPost = "N")
  private lazy val invalidBlpuDueToLogicalStatus = blpu.copy(logicalState = Some(LogicalStatusCode.historical))
  private lazy val invalidBlpuDueToBlpuStatus = blpu.copy(blpuState = Some(BlpuStateCode.noLongerExists))
  private lazy val invalidBlpuDueToEndDates = blpu.copy(endDate = Some(new DateTime()))

  private lazy val blpuProvisonal = BLPU(
    "uprn",
    Some(BlpuStateCode.inUse),
    Some(LogicalStatusCode.provisional),
    1.1,
    2.2,
    1234,
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

  private lazy val lpiWithNoSuffix = lpi.copy(saoStartSuffix = None, saoEndSuffix = None, paoStartSuffix = None, paoEndSuffix = None)

  private lazy val lpiWithNoNumbers = lpi.copy(saoStartNumber = None, saoEndNumber = None, paoStartNumber = None, paoEndNumber = None)

  private lazy val lpiWithSaoStartAndPaoText = LPI(
    "uprn",
    "usrn",
    Some(LogicalStatusCode.approved),
    startDate,
    None,
    lastUpdatedDate,
    None,
    None,
    None,
    None,
    Some("pao text"),
    Some("sao start number"),
    Some("sao start suffix"),
    None,
    None,
    None,
    Some("area name"),
    Some(true)
  )

  private lazy val lpiWithNoSaoFieldsAndNoPaoText = LPI(
    "uprn",
    "usrn",
    Some(LogicalStatusCode.approved),
    startDate,
    None,
    lastUpdatedDate,
    Some("1"),
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    Some("area name"),
    Some(true)
  )

  private lazy val lpiProvisional = LPI(
    "uprn",
    "usrn",
    Some(LogicalStatusCode.provisional),
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

  private lazy val lpiWithEndDate = LPI(
    "uprn",
    "usrn",
    Some(LogicalStatusCode.approved),
    startDate,
    Some(endDate),
    lastUpdatedDate,
    Some("pao start number - end"),
    Some("pao start suffix - end"),
    Some("pao end number - end"),
    Some("pao end suffix - end"),
    Some("pao text - end"),
    Some("sao start number - end"),
    Some("sao start suffix - end"),
    Some("sao end number - end"),
    Some("sao end suffix - end"),
    Some("sao text - end"),
    Some("area name - end"),
    Some(true)
  )

  private lazy val classification = Classification("uprn", "code", startDate, None, lastUpdatedDate)
  private lazy val organisation = Organisation("uprn", "organisation", startDate, None, lastUpdatedDate)
  private lazy val street = Street("usrn", Some(StreetRecordTypeCode.numberedStreet), Some(StreetStateCode.open), Some(StreetSurfaceCode.mixed), Some(StreetClassificationCode.footpath), startDate, None, endDate)
  private lazy val streetDescriptor = StreetDescriptor("usrn", "street name", "locality", "town", "area")
  private lazy val streetMap = Map("usrn" -> List(street))
  private lazy val streetDescriptorMap = Map("usrn" -> streetDescriptor)
}
