package uk.gov.gds.model

import uk.gov.gds.model.CodeLists.{BlpuStateCode, LogicalStatusCode}
import uk.gov.gds.logging.Logging
import uk.gov.gds.MongoConnection
import uk.gov.gds.model._


object AddressBuilder extends Logging {


  def geographicAddressToSimpleAddress(addressWrapper: AddressBaseWrapper)(implicit mongo: Option[MongoConnection]) = {

    logger.info("Getting street descritptor for " + addressWrapper.lpis.head.usrn)

    val streetDescriptor: Option[StreetDescriptor] = mongo.get.streetForUsrn(addressWrapper.lpis.head.usrn)

    streetDescriptor match {
      case Some(street) => {
        val lpis = filerOutIneligibleLPIs(addressWrapper.lpis)

        if (!isValidBLPU(addressWrapper.blpu)) {
          logger.info("Not eligible BLPU " + addressWrapper.blpu.uprn)
          None
        }
        else if (lpis.size <= 0) {
          logger.info("No eligible LPI for " + addressWrapper.blpu.uprn)
          None
        } // No LPI for this address
        else if (lpis.size > 1) {
          logger.info("too many eligble LPIs for " + addressWrapper.blpu.uprn)
          None
        }
        else {
          val lpi = lpis.head

          val streetPrefix = constructStreetAddressPrefix(lpi)

          val streetDescription = street.streetDescription
          val locality = street.localityName
          val town = street.townName
          val area = if (
            town.isDefined &&
              !town.equals(street.administrativeArea)) {
            Some(street.administrativeArea)
          } else {
            None
          }

          val location = Location(addressWrapper.blpu.xCoordinate, addressWrapper.blpu.yCoordinate)
          val presentation = Presentation(
            property = constructProperty(lpi),
            streetAddress = String.format("%s %s", streetPrefix, streetDescription).trim,
            locality = locality,
            town = town,
            area = area,
            postcode = addressWrapper.blpu.postcode,
            uprn = addressWrapper.uprn
          )


          val details = Details(
            blpuCreatedAt = addressWrapper.blpu.startDate,
            blpuUpdatedAt = addressWrapper.blpu.lastUpdated,
            classification = addressWrapper.classifications.head.classificationCode,
            status = addressWrapper.blpu.blpuState.map(pp => pp.toString),
            state = addressWrapper.blpu.logicalState.map(pp => pp.toString),
            isPostalAddress = addressWrapper.blpu.canReceivePost,
            isResidential = addressWrapper.classifications.head.isResidential,
            isCommercial = !addressWrapper.classifications.head.isResidential
          )

          Some(Address(
            houseName = lpis.head.paoText,
            houseNumber = Some(streetPrefix),
            gssCode = addressWrapper.blpu.localCustodianCode.toString,
            postcode = addressWrapper.blpu.postcode.toLowerCase.replaceAll(" ", ""),
            presentation = presentation,
            location = location,
            details = details
          ))


        }
      }
      case _ => None
    }
  }

  private def filerOutIneligibleLPIs(lpis: List[LPI]) = lpis.filter(lpi => !lpi.endDate.isDefined && lpi.logicalState.get.equals(LogicalStatusCode.approved))

  def isValidBLPU(blpu: BLPU) = !List(
    blpu.logicalState.getOrElse(false).equals(LogicalStatusCode.approved), // MUST have a logical state and it MUST be 'approved'
    blpu.blpuState.getOrElse(false).equals(BlpuStateCode.inUse), // MUST have a BLPU state and it MUST be 'in use'
    !blpu.endDate.isDefined, // MUST not have an end date
    blpu.canReceivePost // must be able to receive post
  ).contains(false)

  def constructProperty(lpi: LPI) = {
    val formatted = List(formatStartAndEndSuffixes(lpi.saoStartNumber, lpi.saoStartSuffix, lpi.saoEndNumber, lpi.saoEndSuffix), lpi.saoText, lpi.paoText).flatten.mkString(" ")
    if (formatted.isEmpty) None
    else Some(formatted)
  }

  def constructStreetAddressPrefix(lpi: LPI) = List(formatStartAndEndSuffixes(lpi.paoStartNumber, lpi.paoStartSuffix, lpi.paoEndNumber, lpi.paoEndSuffix)).flatten.mkString(" ")

  private def formatStartAndEndSuffixes(startNumber: Option[String], startSuffix: Option[String], endNumber: Option[String], endSuffix: Option[String]) = {
    val start = if (startNumber.isDefined) List(startNumber, startSuffix).flatten.mkString("") else ""
    val end = if (endNumber.isDefined) List(endNumber, endSuffix).flatten.mkString("") else ""

    if (!start.isEmpty && !end.isEmpty) Some(start + "-" + end)
    else if (!start.isEmpty && end.isEmpty) Some(start)
    else None
  }
}
