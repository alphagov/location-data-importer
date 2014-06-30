package uk.gov.gds.location.importer.helpers

import org.joda.time.DateTime
import uk.gov.gds.location.importer.model._
import uk.gov.gds.location.importer.model.CodeLists._
import scala.Some

object TestHelpers {
  lazy val startDate = new DateTime().minusDays(100)
  lazy val lastUpdatedDate = new DateTime().minusDays(50)
  lazy val endDate = new DateTime().minusDays(50)


  def deliveryPoint(uprn: String) = DeliveryPoint(
    uprn,
    Some("subBuildingName"),
    Some("buildingName"),
    Some("buildingNumber"),
    Some("dependantThoroughfareName"),
    Some("thoroughfareName"),
    Some("doubleDependantLocality"),
    Some("dependantLocality"),
    "POSTCODE",
    startDate,
    None,
    lastUpdatedDate
  )

  def blpu(uprn: String) = BLPU(
    uprn,
    Some(BlpuStateCode.inUse),
    Some(LogicalStatusCode.approved),
    1.1,
    2.2,
    "1234",
    startDate,
    None,
    lastUpdatedDate,
    "S",
    "postcode")


  def lpi(uprn: String, usrn: String) = LPI(
    uprn,
    usrn,
    Some(LogicalStatusCode.approved),
    startDate,
    None,
    lastUpdatedDate,
    Some("1"),
    Some("a"),
    Some("2"),
    Some("b"),
    Some("pao text"),
    Some("3"),
    Some("c"),
    Some("4"),
    Some("d"),
    Some("sao text"),
    Some("area name"),
    Some(true),
    "ENG"
  )


  def street(usrn: String) = Street(usrn, Some(StreetRecordTypeCode.numberedStreet), Some(StreetStateCode.open), Some(StreetSurfaceCode.mixed), Some(StreetClassificationCode.allVehicles), startDate, None, lastUpdatedDate)

  def streetDescriptor(usrn: String) = StreetDescriptor(usrn, "description", Some("locality"), Some("town"), "admin area", "ENG")

  def classification(uprn: String) = Classification(uprn, "code", startDate, None, lastUpdatedDate, "primaryUse", Some("secondaryUse"))

  def organisation(uprn: String) = Organisation(uprn, "organisation", startDate, None, lastUpdatedDate)

  def streetWithDescription(fileName: String, streetDescriptor: StreetDescriptor, s: Street) = StreetWithDescription(
    streetDescriptor.usrn,
    streetDescriptor.streetDescription,
    streetDescriptor.localityName,
    streetDescriptor.townName,
    streetDescriptor.administrativeArea,
    s.recordType.map(r => r.toString),
    s.state.map(r => r.toString),
    s.surface.map(r => r.toString),
    s.classification.map(r => r.toString),
    fileName
  )

  def codePoint = CodePoint("postcode", "country", "district", "name")
}
