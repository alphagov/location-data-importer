package uk.gov.gds.model

import org.joda.time.DateTime
import uk.gov.gds.model.CodeLists.{LogicalStatusCode, BlpuStateCode}
import uk.gov.gds.model.CodeLists.BlpuStateCode.BlpuStateCode
import uk.gov.gds.model.CodeLists.LogicalStatusCode.LogicalStatusCode


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
                 )  {

  def isExpired = endDate.isDefined
}

object BLPU {
  val recordIdentifier = 21

  val uprnIndex = 3
  val logicalStateIndex = 4
  val blpuStateIndex = 5
  val xCoordinateIndex = 8
  val yCoordinateIndex = 9
  val localCustodianCodeIndex = 11
  val startDateIndex = 12
  val endDateIndex = 13
  val updatedDateIndex = 14
  val postcodeIndex = 17

  def fromLineOfCsv(csvLine: List[String]) = {
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




