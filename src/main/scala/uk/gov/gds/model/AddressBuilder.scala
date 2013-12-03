package uk.gov.gds.model

import uk.gov.gds.model.CodeLists.{BlpuStateCode, LogicalStatusCode}
import uk.gov.gds.logging.Logging
import uk.gov.gds.MongoConnection
import uk.gov.gds.logging.Reporter.report
import uk.gov.gds.logging.InvalidBlpuError
import uk.gov.gds.logging.NoStreetForBlpuError


object AddressBuilder extends Logging {

  import formatters._

  def geographicAddressToSimpleAddress(addressWrapper: AddressBaseWrapper)(implicit mongo: Option[MongoConnection], fileName: String) = {
    val streetDescriptor: Option[StreetWithDescription] = mongo.flatMap(_.streetForUsrn(addressWrapper.lpi.usrn))

    streetDescriptor match {

      case Some(street) =>
        if (!isValidBLPU(addressWrapper.blpu)) {
          report(fileName, InvalidBlpuError, addressWrapper.uprn)
          None
        }
        else
          Some(Address(
            houseName = addressWrapper.lpi.paoText,
            houseNumber = constructStreetAddressPrefixFrom(addressWrapper.lpi),
            gssCode = addressWrapper.blpu.localCustodianCode.toString,
            postcode = addressWrapper.blpu.postcode.toLowerCase.replaceAll(" ", ""),
            presentation = presentation(addressWrapper.blpu, addressWrapper.lpi, street),
            location = location(addressWrapper.blpu),
            details = details(addressWrapper)
          ))
      case _ => {
        report(fileName, NoStreetForBlpuError, addressWrapper.uprn)
        None
      }

    }
  }

  /*
    Model class builders
   */
  def details(addressWrapper: AddressBaseWrapper) = Details(
    blpuCreatedAt = addressWrapper.blpu.startDate,
    blpuUpdatedAt = addressWrapper.blpu.lastUpdated,
    classification = addressWrapper.classification.classificationCode,
    status = addressWrapper.blpu.blpuState.map(pp => pp.toString),
    state = addressWrapper.blpu.logicalState.map(pp => pp.toString),
    isPostalAddress = addressWrapper.blpu.canReceivePost,
    isResidential = addressWrapper.classification.isResidential,
    isCommercial = !addressWrapper.classification.isResidential,
    usrn = addressWrapper.lpi.usrn
  )

  def location(blpu: BLPU) = Location(blpu.xCoordinate, blpu.yCoordinate)

  def presentation(blpu: BLPU, lpi: LPI, street: StreetWithDescription) = {
    Presentation(
      property = constructPropertyFrom(lpi),
      streetAddress = constructStreetAddressFrom(lpi, street),
      locality = street.localityName,
      town = street.townName,
      area = if (street.townName.isDefined && !street.townName.equals(street.administrativeArea)) Some(street.administrativeArea) else None,
      postcode = blpu.postcode,
      uprn = blpu.uprn
    )
  }

  /*
    BLPU checker
   */
  def isValidBLPU(blpu: BLPU) = !List(
    // blpu.logicalState.getOrElse(false).equals(LogicalStatusCode.approved), // MUST have a logical state and it MUST be 'approved'
    // blpu.blpuState.getOrElse(false).equals(BlpuStateCode.inUse), // MUST have a BLPU state and it MUST be 'in use'
    !blpu.endDate.isDefined // MUST not have an end date
    // blpu.canReceivePost // must be able to receive post
  ).contains(false)

}

object formatters {
  /*
    Various address field formatters
   */
  def constructPropertyFrom(lpi: LPI) = {
    val formatted = List(formatStartAndEndNumbersAndSuffixes(lpi.saoStartNumber, lpi.saoStartSuffix, lpi.saoEndNumber, lpi.saoEndSuffix), lpi.saoText, lpi.paoText).flatten.mkString(" ")
    if (formatted.isEmpty) None
    else Some(formatted)
  }

  def constructStreetAddressFrom(lpi: LPI, street: StreetWithDescription) =
    if (street.recordType.get.equals("officiallyDesignated"))
      Some(String.format("%s %s", constructStreetAddressPrefixFrom(lpi).getOrElse(""), street.streetDescription).trim)
    else
      None

  def constructStreetAddressPrefixFrom(lpi: LPI) = formatStartAndEndNumbersAndSuffixes(lpi.paoStartNumber, lpi.paoStartSuffix, lpi.paoEndNumber, lpi.paoEndSuffix)

  def formatStartAndEndNumbersAndSuffixes(startNumber: Option[String], startSuffix: Option[String], endNumber: Option[String], endSuffix: Option[String]) = {
    val start = if (startNumber.isDefined) List(startNumber, startSuffix).flatten.mkString("") else ""
    val end = if (endNumber.isDefined) List(endNumber, endSuffix).flatten.mkString("") else ""

    startNumber.isDefined && endNumber.isDefined match {
      case true => Some(start + "-" + end)
      case false if startNumber.isDefined => Some(start)
      case _ => None
    }
  }
}
