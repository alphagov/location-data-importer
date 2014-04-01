package uk.gov.gds.location.importer.conversions

import org.specs2.mutable.Specification

/**
 * This test is a little fudged as getting an example lat/long conversion online to trust was difficult.
 * Correspondingly the accuracy of the latlong is to 4 decimal places only.
 */
class EastingNorthingToLatLongConvertorTest extends Specification {

  "Easting Northing to Lat Long Converter" should {
    "be able to convert known easting and northing to correct lat long" in {
      val easting = 453295.59
      val northing = 349363.45

      val roundedLat = BigDecimal(EastingNorthingToLatLongConvertor.gridReferenceToLatLong(easting, northing).lat).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble
      val roundedLong = BigDecimal(EastingNorthingToLatLongConvertor.gridReferenceToLatLong(easting, northing).long).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble

      roundedLat must beEqualTo(53.0389)
      roundedLong must beEqualTo(-1.2066)
    }
  }
}