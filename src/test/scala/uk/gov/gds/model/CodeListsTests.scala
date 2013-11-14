package uk.gov.gds.model

import org.specs2.mutable.Specification

import CodeLists._
import CodeLists.BlpuStateCode._
import CodeLists.LogicalStatusCode._

class CodeListsTests extends Specification {

  "BLPU State Code lists" should {
    "be constructable from their id" in {

      BlpuStateCode.forId(1) should beEqualTo(Some(underConstruction))
      BlpuStateCode.forId(2) should beEqualTo(Some(inUse))
      BlpuStateCode.forId(3) should beEqualTo(Some(unoccupied))
      BlpuStateCode.forId(4) should beEqualTo(Some(noLongerExists))
      BlpuStateCode.forId(6) should beEqualTo(Some(planningPermissionGranted))
    }

    "generate None if no id matches" in {
      BlpuStateCode.forId(99) should beEqualTo(None)
    }
  }

  "Logical State Code lists" should {
    "be constructable from their id" in {

      LogicalStatusCode.forId(1) should beEqualTo(Some(approved))
      LogicalStatusCode.forId(3) should beEqualTo(Some(alternative))
      LogicalStatusCode.forId(6) should beEqualTo(Some(provisional))
      LogicalStatusCode.forId(8) should beEqualTo(Some(historical))
    }

    "generate None if no id matches" in {
      LogicalStatusCode.forId(99) should beEqualTo(None)
    }
  }
}
