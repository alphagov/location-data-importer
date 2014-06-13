package uk.gov.gds.location.importer.conversions

import uk.gov.gds.location.importer.logging._
import uk.gov.gds.location.importer.model._
import uk.gov.gds.location.importer.model.AddressBaseWrapper
import uk.gov.gds.location.importer.model.Location
import uk.gov.gds.location.importer.model.Details
import uk.gov.gds.location.importer.model.Presentation
import uk.gov.gds.location.importer.model.OrderingHelpers
import uk.gov.gds.location.importer.model.StreetWithDescription
import uk.gov.gds.location.importer.model.Address
import uk.gov.gds.location.importer.model.CodeLists.StreetRecordTypeCode
import LocalAuthorities._

/**
 * Object to take lists of address base types and convert to our address / street model
 */
object AddressBaseToLocateConvertor extends Logging {

  import formatters._

  implicit private def stringToOptionalInt(v: String): Option[Int] = {
    try {
      Some(Integer.valueOf(v))
    } catch {
      case e: Exception => None
    }
  }

  def toLocateAddress(addressWrapper: AddressBaseWrapper, fileName: String) = {
    val streetDescriptor = AllTheStreets.allTheStreets.get(addressWrapper.lpi.usrn)

    streetDescriptor match {
      case Some(street) => {
        localAuthoritiesByCustodianCode.get(addressWrapper.blpu.localCustodianCode) match {
          case Some(localAuthority) => address(addressWrapper, localAuthority, street, fileName)
          case _ =>
            logger.error(String.format("No localAuthority found for address: BLPU [%s] POSTCODE [%s] FILENAME [%s] custodian code [%s]", addressWrapper.uprn, addressWrapper.blpu.postcode, fileName, addressWrapper.blpu.localCustodianCode))
            None
        }
      }
      case _ =>
        logger.error(String.format("No street found for address: BLPU [%s] POSTCODE [%s] FILENAME [%s]", addressWrapper.uprn, addressWrapper.blpu.postcode, fileName))
        None
    }
  }

  def address(addressWrapper: AddressBaseWrapper, localAuthority: LocalAuthority, street: StreetWithDescription, fileName: String) = {

    /**
     * Check custodian code derived gssCode against code point if code point resolves to postcode
     * Don't use this for persitance- simple a matter of logging
     */
    AllTheCodePoints.codePoints.get(addressWrapper.blpu.postcode.toLowerCase.replaceAll(" ", "")) match {
      case Some(codePoint) => checkGssCodeWithCustodianCode(addressWrapper.blpu, codePoint._1, fileName)
      case _ =>
    }

    Some(Address(
      gssCode = localAuthority.gssCode,
      country = Countries.countryForGssCode(localAuthority.gssCode),
      uprn = addressWrapper.blpu.uprn,
      postcode = lowercase(stripAllWhitespace(addressWrapper.blpu.postcode)),
      presentation = presentation(addressWrapper.blpu, addressWrapper.lpi, street, addressWrapper.deliveryPoint),
      location = location(addressWrapper.blpu),
      details = details(addressWrapper, fileName),
      ordering = Some(ordering(addressWrapper))
    ))
  }

  /**
   * Address base objects have a custodian code, which maps to a GSSCode
   * Compare the Codepoint derived GSSCode with the Custodian code version
   * Using custodian code on any variation
   * @param custodianCode
   * @param gssCode
   * @param fileName
   * @return   
   */
  def checkGssCodeWithCustodianCode(blpu: BLPU, gssCode: String, fileName: String) = {
    localAuthoritiesByCustodianCode.get(blpu.localCustodianCode) match {
      case Some(la) if la.gssCode.equalsIgnoreCase(gssCode) => gssCode
      case Some(la) if !la.gssCode.equalsIgnoreCase(gssCode) => {
        logger.info("GSSCode and Custodian code mismatch: file [%s] uprn [%s] custodian code[%s], la.gssCode [%s] codepoint.gssCode [%s]".format(fileName, blpu.uprn, blpu.localCustodianCode, la.gssCode, gssCode))
        la.gssCode
      }
      case _ => gssCode
    }
  }

  /*
    Model class builders
   */

  def ordering(addressWrapper: AddressBaseWrapper) = OrderingHelpers(
    paoStartNumber = addressWrapper.lpi.paoStartNumber.flatMap(n => n),
    paoStartSuffix = addressWrapper.lpi.paoStartSuffix.flatMap(n => n),
    paoEndNumber = addressWrapper.lpi.paoEndNumber.flatMap(n => n),
    paoEndSuffix = addressWrapper.lpi.paoEndSuffix.flatMap(n => n),
    saoStartNumber = addressWrapper.lpi.saoStartNumber.flatMap(n => n),
    saoStartSuffix = addressWrapper.lpi.saoStartSuffix.flatMap(n => n),
    saoEndNumber = addressWrapper.lpi.saoEndNumber.flatMap(n => n),
    saoEndSuffix = addressWrapper.lpi.saoEndSuffix.flatMap(n => n),
    paoText = addressWrapper.lpi.paoText.map(v => stripAllWhitespace(lowercase(v))),
    saoText = addressWrapper.lpi.saoText.map(v => stripAllWhitespace(lowercase(v)))
  )

  def location(blpu: BLPU) = Location(blpu.lat, blpu.long)

  def presentation(blpu: BLPU, lpi: LPI, street: StreetWithDescription, deliveryPoint: Option[DeliveryPoint]) = {
    Presentation(
      property = toSentenceCase(constructPropertyFrom(lpi)),
      street = toSentenceCase(constructStreetAddressFrom(lpi, street, deliveryPoint)),
      locality = toSentenceCase(street.localityName),
      town = toSentenceCase(street.townName),
      area = constructArea(street),
      postcode = blpu.postcode
    )
  }

  def constructArea(street: StreetWithDescription) =
    toSentenceCase(if (!street.townName.isDefined) street.administrativeArea
    else if (street.townName.isDefined && street.townName.get.equals(street.administrativeArea)) None
    else street.administrativeArea)

  def details(addressWrapper: AddressBaseWrapper, filename: String) = Details(
    blpuCreatedAt = addressWrapper.blpu.startDate,
    blpuUpdatedAt = addressWrapper.blpu.lastUpdated,
    classification = addressWrapper.classification.classificationCode,
    status = addressWrapper.blpu.blpuState.map(pp => pp.toString),
    state = addressWrapper.blpu.logicalState.map(pp => pp.toString),
    isPostalAddress = addressWrapper.blpu.canReceivePost,
    isResidential = addressWrapper.classification.isResidential,
    isCommercial = addressWrapper.classification.isCommercial,
    isHigherEducational = addressWrapper.classification.isEducational,
    isElectoral = (addressWrapper.classification.isEducational || addressWrapper.classification.isResidential) && addressWrapper.blpu.canReceivePost,
    usrn = addressWrapper.lpi.usrn,
    file = filename,
    organisation = toSentenceCase(addressWrapper.organisation.map(org => org.organistation)),
    primaryClassification = addressWrapper.classification.primaryUse,
    secondaryClassification = addressWrapper.classification.secondaryUse
  )

}

/**
 * Various utilities for string / field conversions / creations
 */
object formatters extends Logging {

  def constructPropertyFrom(lpi: LPI) = {
    val formatted = (
      toSentenceCase(lpi.saoText).toList ++
        formatStartAndEndNumbersAndSuffixes(lpi.saoStartNumber, lpi.saoStartSuffix, lpi.saoEndNumber, lpi.saoEndSuffix).toList ++
        toSentenceCase(lpi.paoText).toList
      ).mkString(" ")

    if (formatted.isEmpty) None
    else Some(formatted)
  }

  /**
   * Build street address (ie 4 High Street).
   * If Street Description is of classification unofficial or description then use delivery point fields
   * @param lpi
   * @param street
   * @param deliveryPoint
   * @return
   */
  def constructStreetAddressFrom(lpi: LPI, street: StreetWithDescription, deliveryPoint: Option[DeliveryPoint]) = {

    if (invalidStreetDescription(street)) {
      logger.info("Using delivery point: street classification [%s] uprn [%s]".format(street.recordType, lpi.uprn))
      Some(String.format("%s %s", deliveryPoint.get.buildingNumber, deliveryPoint.get.thoroughfareName))
    }
    else
      Some(String.format("%s %s", constructStreetAddressPrefixFrom(lpi).getOrElse(""), toSentenceCase(street.streetDescription).get).trim)
  }

  def invalidStreetDescription(street: StreetWithDescription) = !street.recordType.isDefined || StreetRecordTypeCode.isUnofficialStreet(street.recordType.get) || StreetRecordTypeCode.isDescription(street.recordType.get)

  def constructStreetAddressPrefixFrom(lpi: LPI) = formatStartAndEndNumbersAndSuffixes(lpi.paoStartNumber, lpi.paoStartSuffix, lpi.paoEndNumber, lpi.paoEndSuffix)

  /**
   * Rules on this are:
   * If BOTH start and end INCLUDE BOTH
   * If ANY suffixes INCLUDE ONLY IF PARTNERED WITH A NUMBER
   * If ONLY start include START
   * If ONLY end include END
   * If ONLY suffixes include NOTHING
   * @param startNumber
   * @param startSuffix
   * @param endNumber
   * @param endSuffix
   * @return
   */
  def formatStartAndEndNumbersAndSuffixes(startNumber: Option[String], startSuffix: Option[String], endNumber: Option[String], endSuffix: Option[String]) = {
    val start = if (startNumber.isDefined) List(startNumber, startSuffix).flatten.mkString("") else ""
    val end = if (endNumber.isDefined) List(endNumber, endSuffix).flatten.mkString("") else ""

    startNumber.isDefined && endNumber.isDefined match {
      case true => Some(start + "-" + end)
      case false if startNumber.isDefined => Some(start)
      case false if endNumber.isDefined => Some(end)
      case _ => None
    }
  }

  def toSentenceCase(field: Option[String]) = field.map(f => (f toLowerCase) split (" ") map (_.capitalize) mkString (" "))

  def stripAllWhitespace(from: String) = from replaceAll(" ", "") trim

  def lowercase(from: String) = from toLowerCase
}
