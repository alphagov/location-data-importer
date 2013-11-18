package uk.gov.gds.model

import org.joda.time.DateTime
import uk.gov.gds.model.CodeLists._
import uk.gov.gds.model.CodeLists.BlpuStateCode.BlpuStateCode
import uk.gov.gds.model.CodeLists.LogicalStatusCode.LogicalStatusCode
import uk.gov.gds.model.CodeLists.StreetRecordTypeCode.StreetRecordTypeCode
import uk.gov.gds.model.CodeLists.StreetStateCode.StreetStateCode
import uk.gov.gds.model.CodeLists.StreetSurfaceCode.StreetSurfaceCode
import uk.gov.gds.model.CodeLists.StreetClassificationCode.StreetClassificationCode
import uk.gov.gds.model.CodeLists.StreetRecordTypeCode.StreetRecordTypeCode
import uk.gov.gds.model.CodeLists.StreetStateCode.StreetStateCode
import uk.gov.gds.model.CodeLists.BlpuStateCode.BlpuStateCode
import uk.gov.gds.model.CodeLists.BlpuStateCode
import uk.gov.gds.model.CodeLists.StreetRecordTypeCode.StreetRecordTypeCode
import uk.gov.gds.model.CodeLists.StreetRecordTypeCode
import uk.gov.gds.model.CodeLists.StreetStateCode
import uk.gov.gds.model.CodeLists.StreetStateCode.StreetStateCode
import uk.gov.gds.model.CodeLists.LogicalStatusCode.LogicalStatusCode
import uk.gov.gds.model.CodeLists.LogicalStatusCode


case class AddressBaseWrapper(blpu: BLPU, lpis: List[LPI]) {
  lazy val uprn = blpu.uprn
}

trait AddressBase

trait AddressBaseHelpers[T <: AddressBase] {
  val recordIdentifier: String
  val requiredCsvColumns: Int

  def isValidCsvLine(csvLine: List[String]) = csvLine(0) == recordIdentifier && csvLine.size == requiredCsvColumns

  def fromCsvLine(csvLine: List[String]): T

}

/* Basic Land and Property Unit */
case class BLPU(
                 uprn: String,
                 blpuState: Option[BlpuStateCode],
                 logicalState: Option[LogicalStatusCode],
                 xCoordinate: Double,
                 yCoordinate: Double,
                 localCustodianCode: Int,
                 startDate: DateTime,
                 endDate: Option[DateTime],
                 lastUpdated: DateTime,
                 postcode: String
                 ) extends AddressBase

object BLPU extends AddressBaseHelpers[BLPU] {
  val recordIdentifier = "21"
  val requiredCsvColumns = 19

  private val uprnIndex = 3
  private val logicalStateIndex = 4
  private val blpuStateIndex = 5
  private val xCoordinateIndex = 8
  private val yCoordinateIndex = 9
  private val localCustodianCodeIndex = 11
  private val startDateIndex = 12
  private val endDateIndex = 13
  private val updatedDateIndex = 14
  private val postcodeIndex = 17

  def fromCsvLine(csvLine: List[String]) = {
    BLPU(
      csvLine(uprnIndex),
      BlpuStateCode.forId(csvLine(blpuStateIndex)),
      LogicalStatusCode.forId(csvLine(logicalStateIndex)),
      csvLine(xCoordinateIndex),
      csvLine(yCoordinateIndex),
      csvLine(localCustodianCodeIndex),
      csvLine(startDateIndex),
      csvLine(endDateIndex),
      csvLine(updatedDateIndex),
      csvLine(postcodeIndex)
    )
  }
}

case class LPI(
                uprn: String,
                usrn: String,
                logicalState: Option[LogicalStatusCode],
                startDate: DateTime,
                endDate: Option[DateTime],
                lastUpdated: DateTime,
                paoStartNumber: Option[String],
                paoStartSuffix: Option[String],
                paoEndNumber: Option[String],
                paoEndSuffix: Option[String],
                paoText: Option[String],
                saoStartNumber: Option[String],
                saoStartSuffix: Option[String],
                saoEndNumber: Option[String],
                saoEndSuffix: Option[String],
                saoText: Option[String],
                areaName: Option[String],
                officialAddress: Option[Boolean]
                ) extends AddressBase

object LPI extends AddressBaseHelpers[LPI] {
  val recordIdentifier = "24"
  val requiredCsvColumns = 26

  private val uprnIndex = 3
  private val logicalStateIndex = 6
  private val startDateIndex = 7
  private val endDateIndex = 8
  private val updatedDateIndex = 9
  private val paoStartNumber = 16
  private val paoStartSuffix = 17
  private val paoEndNumber = 18
  private val paoEndSuffix = 19
  private val paoText = 20
  private val saoStartNumber = 11
  private val saoStartSuffix = 12
  private val saoEndNumber = 13
  private val saoEndSuffix = 14
  private val saoText = 15
  private val usrnIndex = 21
  private val areaNameIndex = 23
  private val officialFlagIndex = 25


  def fromCsvLine(csvLine: List[String]) = LPI(
    csvLine(uprnIndex),
    csvLine(usrnIndex),
    LogicalStatusCode.forId(csvLine(logicalStateIndex)),
    csvLine(startDateIndex),
    csvLine(endDateIndex),
    csvLine(updatedDateIndex),
    csvLine(paoStartNumber),
    csvLine(paoStartSuffix),
    csvLine(paoEndNumber),
    csvLine(paoEndSuffix),
    csvLine(paoText),
    csvLine(saoStartNumber),
    csvLine(saoStartSuffix),
    csvLine(saoEndNumber),
    csvLine(saoEndSuffix),
    csvLine(saoText),
    csvLine(areaNameIndex),
    csvLine(officialFlagIndex)
  )
}

case class Street(usrn: String,
                  recordType: Option[StreetRecordTypeCode],
                  state: Option[StreetStateCode],
                  surface: Option[StreetSurfaceCode],
                  classification: Option[StreetClassificationCode],
                  startDate: DateTime,
                  endDate: Option[DateTime],
                  lastUpdated: DateTime
                   ) extends AddressBase

object Street extends AddressBaseHelpers[Street] {
  val recordIdentifier = "11"

  val requiredCsvColumns = 20

  private val usrnIndex = 3
  private val recordTypeIndex = 4
  private val stateIndex = 6
  private val surfaceIndex = 8
  private val classificationIndex = 9
  private val startDateIndex = 11
  private val endDateIndex = 12
  private val updatedDateIndex = 13

  def fromCsvLine(csvLine: List[String]) = Street(
    csvLine(usrnIndex),
    StreetRecordTypeCode.forId(csvLine(recordTypeIndex)),
    StreetStateCode.forId(csvLine(stateIndex)),
    StreetSurfaceCode.forId(csvLine(surfaceIndex)),
    StreetClassificationCode.forId(csvLine(classificationIndex)),
    csvLine(startDateIndex),
    csvLine(endDateIndex),
    csvLine(updatedDateIndex)
  )
}

case class StreetDescriptor(
                             usrn: String,
                             streetDescription: String,
                             localityName: String,
                             townName: String,
                             administrativeArea: String
                             ) extends AddressBase

object StreetDescriptor extends AddressBaseHelpers[StreetDescriptor] {
  val recordIdentifier = "15"
  val requiredCsvColumns = 9

  private val usrnIndex = 3
  private val streetDescriptionIndex = 4
  private val localityNameIndex = 5
  private val townNameIndex = 6
  private val administrativeAreaIndex = 7


  def fromCsvLine(csvLine: List[String]) = StreetDescriptor(
      csvLine(usrnIndex),
      csvLine(streetDescriptionIndex),
      csvLine(localityNameIndex),
      csvLine(townNameIndex),
      csvLine(administrativeAreaIndex)
  )
}


