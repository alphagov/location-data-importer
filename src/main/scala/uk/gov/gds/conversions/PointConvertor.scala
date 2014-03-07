package uk.gov.gds.conversions

import org.geotools.referencing.ReferencingFactoryFinder
import org.geotools.referencing.operation.DefaultCoordinateOperationFactory
import org.geotools.geometry.GeneralDirectPosition

object PointConvertor {

  private val crs = ReferencingFactoryFinder.getCRSAuthorityFactory("EPSG", null)
  private val egs84crs = crs.createCoordinateReferenceSystem("4326")
  private val osbgr = crs.createCoordinateReferenceSystem("27700")
  private val op = new DefaultCoordinateOperationFactory().createOperation(osbgr, egs84crs)

  def gridReferenceToLatLong(easting: Double, northing: Double) = {
    val eastNorth = new GeneralDirectPosition(518841.2, 168688.3)
    op.getMathTransform().transform(eastNorth, eastNorth)
  }


}
