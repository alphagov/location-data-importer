package uk.gov.gds.model

import org.joda.time.DateTime
import uk.gov.gds.model.CodeLists.{LogicalStatusCode, BlpuStateCode}
import uk.gov.gds.model.CodeLists.BlpuStateCode.BlpuStateCode
import uk.gov.gds.model.CodeLists.LogicalStatusCode.LogicalStatusCode

trait AddressBase

trait AddressBaseHelpers[T <: AddressBase] {
  val recordIdentifier: String
  val requiredCsvColumns: Int

  def isValidCsvLine(csvLine: List[String]) = {
    println(csvLine(0) + " " + requiredCsvColumns + " " + csvLine.size)
    csvLine(0) == recordIdentifier && csvLine.size == requiredCsvColumns
  }

  def fromCsvLine(line: List[String]): T

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



