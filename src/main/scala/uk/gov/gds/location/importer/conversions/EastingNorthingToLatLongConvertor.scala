package uk.gov.gds.location.importer.conversions

import org.geotools.referencing.ReferencingFactoryFinder
import org.geotools.referencing.operation.DefaultCoordinateOperationFactory
import org.geotools.geometry.GeneralDirectPosition
import org.opengis.geometry.DirectPosition

case class LatLong(lat: Double, long: Double)

object LatLong {
  def apply(directPosition: DirectPosition):LatLong = LatLong(directPosition.getCoordinate()(0), directPosition.getCoordinate()(1))
}

object EastingNorthingToLatLongConvertor {

  private val crs = ReferencingFactoryFinder.getCRSAuthorityFactory("EPSG", null)
  private val egs84crs = crs.createCoordinateReferenceSystem("4326")
  private val osbgr = crs.createCoordinateReferenceSystem("27700")
  private val op = new DefaultCoordinateOperationFactory().createOperation(osbgr, egs84crs)

  def gridReferenceToLatLong(easting: Double, northing: Double) = {
    val eastNorth = new GeneralDirectPosition(easting, northing)
    LatLong(op.getMathTransform().transform(eastNorth, eastNorth))
  }
}
