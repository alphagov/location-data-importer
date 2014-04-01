package uk.gov.gds.location.importer.model

import org.specs2.mutable.Specification

class ClassificationCodesTest extends Specification {

  import ClassificationCodes._

  "Classification Codes" should {

    "indicate if classification is residential" in {
      isResidential("RD Anything after ignored") must beTrue
      isResidential("RH Anything after ignored") must beTrue
      isResidential("RI Anything after ignored") must beTrue
      isResidential("RB Anything after ignored") must beFalse
      isResidential("RC Anything after ignored") must beFalse
      isResidential("RG Anything after ignored") must beFalse
      isResidential("A Anything after ignored") must beFalse
    }

    "return correct primary code based on 1st letter of classification" in {
      primaryCodeFor("C Anything after ignored") must beEqualTo(Some("Commercial"))
      primaryCodeFor("L Anything after ignored") must beEqualTo(Some("Land"))
      primaryCodeFor("M Anything after ignored") must beEqualTo(Some("Military"))
      primaryCodeFor("O Anything after ignored") must beEqualTo(Some("Other"))
      primaryCodeFor("P Anything after ignored") must beEqualTo(Some("Parent Shell"))
      primaryCodeFor("R Anything after ignored") must beEqualTo(Some("Residential"))
      primaryCodeFor("U Anything after ignored") must beEqualTo(Some("Unclassified"))
      primaryCodeFor("X Anything after ignored") must beEqualTo(Some("Dual Use"))
      primaryCodeFor("Z Anything after ignored") must beEqualTo(Some("Object of interest"))
    }

    "return None if no matching primary classification" in {
      primaryCodeFor("B Anything after ignored") must beEqualTo(None)
    }

    "return None for primary classification if classification is zero length string" in {
      primaryCodeFor("") must beEqualTo(None)
    }

    "return None if secondary classification is string of length < 2" in {
      secondaryCodeFor("C") must beEqualTo(None)
    }

    "return None if secondary classification is unknown" in {
      secondaryCodeFor("CFFDOIFJOIFJ") must beEqualTo(None)
    }

    "return correct secondary code based on 1st 2 letters letter of classification" in {
      secondaryCodeFor("CA Anything after ignored") must beEqualTo(Some("Agriculture"))
      secondaryCodeFor("CB Anything after ignored") must beEqualTo(Some("Ancillary Building"))
      secondaryCodeFor("CC Anything after ignored") must beEqualTo(Some("Community Services"))
      secondaryCodeFor("CE Anything after ignored") must beEqualTo(Some("Education"))
      secondaryCodeFor("CH Anything after ignored") must beEqualTo(Some("Hotel / Motel / Boarding / Guest House"))
      secondaryCodeFor("CI Anything after ignored") must beEqualTo(Some("Industrial Application"))
      secondaryCodeFor("CL Anything after ignored") must beEqualTo(Some("Lesiure"))
      secondaryCodeFor("CM Anything after ignored") must beEqualTo(Some("Medical"))
      secondaryCodeFor("CN Anything after ignored") must beEqualTo(Some("Animal Centre"))
      secondaryCodeFor("CO Anything after ignored") must beEqualTo(Some("Office"))
      secondaryCodeFor("CR Anything after ignored") must beEqualTo(Some("Retail"))
      secondaryCodeFor("CS Anything after ignored") must beEqualTo(Some("Storage"))
      secondaryCodeFor("CT Anything after ignored") must beEqualTo(Some("Transport"))
      secondaryCodeFor("CU Anything after ignored") must beEqualTo(Some("Utility"))
      secondaryCodeFor("CX Anything after ignored") must beEqualTo(Some("Emergency Services"))
      secondaryCodeFor("CZ Anything after ignored") must beEqualTo(Some("Information"))
      secondaryCodeFor("LA Anything after ignored") must beEqualTo(Some("Agricultural Land"))
      secondaryCodeFor("LB Anything after ignored") must beEqualTo(Some("Ancillary Building"))
      secondaryCodeFor("LC Anything after ignored") must beEqualTo(Some("Burial Ground"))
      secondaryCodeFor("LD Anything after ignored") must beEqualTo(Some("Development"))
      secondaryCodeFor("LF Anything after ignored") must beEqualTo(Some("Forestry"))
      secondaryCodeFor("LL Anything after ignored") must beEqualTo(Some("Allotment"))
      secondaryCodeFor("LM Anything after ignored") must beEqualTo(Some("Amenity"))
      secondaryCodeFor("LO Anything after ignored") must beEqualTo(Some("Open Space"))
      secondaryCodeFor("LP Anything after ignored") must beEqualTo(Some("Park"))
      secondaryCodeFor("LU Anything after ignored") must beEqualTo(Some("Unused Land"))
      secondaryCodeFor("LW Anything after ignored") must beEqualTo(Some("Water"))
      secondaryCodeFor("MA Anything after ignored") must beEqualTo(Some("Army"))
      secondaryCodeFor("MB Anything after ignored") must beEqualTo(Some("Ancillary Building"))
      secondaryCodeFor("MF Anything after ignored") must beEqualTo(Some("Air Force"))
      secondaryCodeFor("MG Anything after ignored") must beEqualTo(Some("Defense Estates"))
      secondaryCodeFor("MN Anything after ignored") must beEqualTo(Some("Navy"))
      secondaryCodeFor("OA Anything after ignored") must beEqualTo(Some("Aid To Naviation"))
      secondaryCodeFor("OC Anything after ignored") must beEqualTo(Some("Coastal Protection / Flood Prevention"))
      secondaryCodeFor("OE Anything after ignored") must beEqualTo(Some("Emergency Support"))
      secondaryCodeFor("OF Anything after ignored") must beEqualTo(Some("Street Furniture"))
      secondaryCodeFor("OG Anything after ignored") must beEqualTo(Some("Agricultural Support Objects"))
      secondaryCodeFor("OH Anything after ignored") must beEqualTo(Some("Historical Site"))
      secondaryCodeFor("OI Anything after ignored") must beEqualTo(Some("Industrial Support"))
      secondaryCodeFor("ON Anything after ignored") must beEqualTo(Some("Significant Natural Object"))
      secondaryCodeFor("OO Anything after ignored") must beEqualTo(Some("Ornamental / Cultural Object"))
      secondaryCodeFor("OP Anything after ignored") must beEqualTo(Some("Sport / Leisure Support"))
      secondaryCodeFor("OR Anything after ignored") must beEqualTo(Some("Royal Mail Infrastructure"))
      secondaryCodeFor("OS Anything after ignored") must beEqualTo(Some("Scientific / Observation Support"))
      secondaryCodeFor("OT Anything after ignored") must beEqualTo(Some("Transport Support"))
      secondaryCodeFor("PP Anything after ignored") must beEqualTo(Some("Property Shell"))
      secondaryCodeFor("PS Anything after ignored") must beEqualTo(Some("Street Record"))
      secondaryCodeFor("RB Anything after ignored") must beEqualTo(Some("Ancillary Building"))
      secondaryCodeFor("RC Anything after ignored") must beEqualTo(Some("Car Park Space"))
      secondaryCodeFor("RD Anything after ignored") must beEqualTo(Some("Dwelling"))
      secondaryCodeFor("RG Anything after ignored") must beEqualTo(Some("Garage"))
      secondaryCodeFor("RH Anything after ignored") must beEqualTo(Some("House In Multiple Occupation"))
      secondaryCodeFor("RI Anything after ignored") must beEqualTo(Some("Residential Institution"))
      secondaryCodeFor("UC Anything after ignored") must beEqualTo(Some("Awaiting Classification"))
      secondaryCodeFor("UP Anything after ignored") must beEqualTo(Some("Pending Internal Investigation"))
      secondaryCodeFor("ZA Anything after ignored") must beEqualTo(Some("Archaeological Dig Site"))
      secondaryCodeFor("ZM Anything after ignored") must beEqualTo(Some("Monument"))
      secondaryCodeFor("ZS Anything after ignored") must beEqualTo(Some("Stately Home"))
      secondaryCodeFor("ZU Anything after ignored") must beEqualTo(Some("Underground Feature"))
      secondaryCodeFor("ZW Anything after ignored") must beEqualTo(Some("Place Of Worship"))
    }
  }

}