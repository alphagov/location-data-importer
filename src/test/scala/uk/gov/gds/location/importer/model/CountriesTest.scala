package uk.gov.gds.location.importer.model

import org.specs2.mutable.Specification

class CountriesTest extends Specification {

  "Countries object" should {
    "resolve gsscodes starting with E as England" in {
      Countries.countryForGssCode("E123456789") must beEqualTo("England")
    }

    "resolve gsscodes starting with W as Wales" in {
      Countries.countryForGssCode("W123456789") must beEqualTo("Wales")
    }

    "resolve gsscodes starting with S as Scotland" in {
      Countries.countryForGssCode("S123456789") must beEqualTo("Scotland")
    }
  }
}
