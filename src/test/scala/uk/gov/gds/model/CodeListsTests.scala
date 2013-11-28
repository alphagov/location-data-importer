package uk.gov.gds.model

import org.specs2.mutable.Specification

import CodeLists._
import CodeLists.BlpuStateCode._
import CodeLists.LogicalStatusCode._
import CodeLists.StreetClassificationCode._
import CodeLists.StreetRecordTypeCode._
import CodeLists.StreetStateCode._
import CodeLists.StreetSurfaceCode._

class CodeListsTests extends Specification {

  "BLPU State Code lists" should {
    "be constructable from their id" in {

      BlpuStateCode.forId("1") should beEqualTo(Some(underConstruction))
      BlpuStateCode.forId("2") should beEqualTo(Some(inUse))
      BlpuStateCode.forId("3") should beEqualTo(Some(unoccupied))
      BlpuStateCode.forId("4") should beEqualTo(Some(noLongerExists))
      BlpuStateCode.forId("6") should beEqualTo(Some(planningPermissionGranted))
    }

    "generate None if no id matches" in {
      BlpuStateCode.forId("") should beEqualTo(None)
    }
  }

  "Street Record Type Code lists" should {
    "be constructable from their id" in {

      StreetRecordTypeCode.forId("1") should beEqualTo(Some(officiallyDesignated))
      StreetRecordTypeCode.forId("2") should beEqualTo(Some(streetDescription))
      StreetRecordTypeCode.forId("3") should beEqualTo(Some(numberedStreet))
      StreetRecordTypeCode.forId("4") should beEqualTo(Some(unofficialStreetDescription))
      StreetRecordTypeCode.forId("9") should beEqualTo(Some(descriptionForLLPG))
    }

    "generate None if no id matches" in {
      StreetRecordTypeCode.forId("99") should beEqualTo(None)
    }
  }

  "Logical State Code lists" should {
    "be constructable from their id" in {

      LogicalStatusCode.forId("1") should beEqualTo(Some(approved))
      LogicalStatusCode.forId("3") should beEqualTo(Some(alternative))
      LogicalStatusCode.forId("6") should beEqualTo(Some(provisional))
      LogicalStatusCode.forId("8") should beEqualTo(Some(historical))
    }

    "generate None if no id matches" in {
      LogicalStatusCode.forId("99") should beEqualTo(None)
    }
  }

  "Street Record Type Code" should {
    "be constructable from their id" in {

      StreetRecordTypeCode.forId("1") should beEqualTo(Some(officiallyDesignated))
      StreetRecordTypeCode.forId("2") should beEqualTo(Some(streetDescription))
      StreetRecordTypeCode.forId("3") should beEqualTo(Some(numberedStreet))
      StreetRecordTypeCode.forId("4") should beEqualTo(Some(unofficialStreetDescription))
      StreetRecordTypeCode.forId("9") should beEqualTo(Some(descriptionForLLPG))
    }

    "generate None if no id matches" in {
      StreetRecordTypeCode.forId("99") should beEqualTo(None)
    }
  }

  "Street State Code lists" should {
    "be constructable from their id" in {

      StreetStateCode.forId("1") should beEqualTo(Some(streetUnderConstruction))
      StreetStateCode.forId("2") should beEqualTo(Some(open))
      StreetStateCode.forId("4") should beEqualTo(Some(closed))
    }

    "generate None if no id matches" in {
      StreetStateCode.forId("99") should beEqualTo(None)
    }
  }

  "Street Surface Code lists" should {
    "be constructable from their id" in {

      StreetSurfaceCode.forId("1") should beEqualTo(Some(metalled))
      StreetSurfaceCode.forId("2") should beEqualTo(Some(unMetalled))
      StreetSurfaceCode.forId("3") should beEqualTo(Some(mixed))
    }

    "generate None if no id matches" in {
      StreetSurfaceCode.forId("99") should beEqualTo(None)
    }
  }

  "Street Classification Code lists" should {
    "be constructable from their id" in {

      StreetClassificationCode.forId("4") should beEqualTo(Some(footpath))
      StreetClassificationCode.forId("6") should beEqualTo(Some(cycleway))
      StreetClassificationCode.forId("8") should beEqualTo(Some(allVehicles))
      StreetClassificationCode.forId("9") should beEqualTo(Some(restricted))
      StreetClassificationCode.forId("10") should beEqualTo(Some(bridleway))
    }

    "generate None if no id matches" in {
      StreetClassificationCode.forId("99") should beEqualTo(None)
    }
  }
}
