package uk.gov.gds.location.importer.model

import org.specs2.mutable.Specification

import LocalAuthorities._

class LocalAuthoritiesTests extends Specification {

  "Local Authorities" should {
    "have 380 entries" in {
      localAuthorities.size must beEqualTo(380)
    }

    "names should all match up" in {
      localAuthorities.foreach(la => {
        if(!la.onsName.equalsIgnoreCase(la.osName)) println("%s %s %s".format(la.onsName, la.osName, la.onsName.equalsIgnoreCase(la.osName)))
      })
      localAuthorities.map(la => la.onsName.equalsIgnoreCase(la.osName)).toList must not(contain(false))
    }

    "should be able to get an LA by custodian code" in {
      localAuthoritiesByCustodianCode("9051").gssCode must beEqualTo("S12000033") // Aberdeen
    }

    "should be able to get an LA by gss code" in {
      localAuthoritiesByGssCode("S12000033").custodianCode must beEqualTo("9051") // Aberdeen
    }
  }
}
