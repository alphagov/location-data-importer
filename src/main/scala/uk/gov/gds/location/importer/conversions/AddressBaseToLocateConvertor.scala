package uk.gov.gds.location.importer.conversions

import uk.gov.gds.location.importer.logging._
import uk.gov.gds.location.importer.model._
import org.joda.time.DateTime
import uk.gov.gds.location.importer.model.AddressBaseWrapper
import uk.gov.gds.location.importer.model.Location
import scala.Some
import uk.gov.gds.location.importer.model.Details
import uk.gov.gds.location.importer.model.Presentation
import uk.gov.gds.location.importer.model.OrderingHelpers
import uk.gov.gds.location.importer.model.StreetWithDescription
import uk.gov.gds.location.importer.model.Address
import uk.gov.gds.location.importer.model.CodeLists.StreetRecordTypeCode

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
        val codePoint = AllTheCodePoints.codePoints.get(addressWrapper.blpu.postcode.toLowerCase.replaceAll(" ", ""))
        codePoint match {
          case Some(code) => {
            address(addressWrapper, code._2, code._1, street, fileName)
          }
          case _ => {
            logger.error(String.format("No codepoint found for address: BLPU [%s] POSTCODE [%s] FILENAME [%s]", addressWrapper.uprn, addressWrapper.blpu.postcode, fileName))
            None
          }
        }
      }
      case _ => {
        logger.error(String.format("No street found for address: BLPU [%s] POSTCODE [%s] FILENAME [%s]", addressWrapper.uprn, addressWrapper.blpu.postcode, fileName))
        None
      }

    }
  }

  def address(addressWrapper: AddressBaseWrapper, country: String, gssCode: String, street: StreetWithDescription, fileName: String) = {
    Some(Address(
      houseName = toSentenceCase(addressWrapper.lpi.paoText),
      houseNumber = constructStreetAddressPrefixFrom(addressWrapper.lpi),
      gssCode = gssCode,
      countryCode = country,
      postcode = addressWrapper.blpu.postcode.toLowerCase.replaceAll(" ", ""),
      presentation = presentation(addressWrapper.blpu, addressWrapper.lpi, street),
      location = location(addressWrapper.blpu),
      details = details(addressWrapper, fileName),
      ordering = Some(ordering(addressWrapper))
    ))
  }

  /*
    Model class builders
   */

  def ordering(addressWrapper: AddressBaseWrapper) = OrderingHelpers(
    paoStartNumber = addressWrapper.lpi.paoStartNumber.flatMap(n => n),
    paoEndNumber = addressWrapper.lpi.paoEndNumber.flatMap(n => n),
    saoStartNumber = addressWrapper.lpi.saoStartNumber.flatMap(n => n),
    saoEndNumber = addressWrapper.lpi.saoEndNumber.flatMap(n => n),
    paoText = addressWrapper.lpi.paoText.map(v => stripAllWhitespace(lowercase(v))),
    saoText = addressWrapper.lpi.saoText.map(v => stripAllWhitespace(lowercase(v)))
  )

  def location(blpu: BLPU) = Location(blpu.lat, blpu.long)

  def presentation(blpu: BLPU, lpi: LPI, street: StreetWithDescription) = {
    Presentation(
      property = toSentenceCase(constructPropertyFrom(lpi)),
      street = toSentenceCase(constructStreetAddressFrom(lpi, street)),
      locality = toSentenceCase(street.localityName),
      town = toSentenceCase(street.townName),
      area = if (street.townName.isDefined && !street.townName.get.equals(street.administrativeArea)) toSentenceCase(Some(street.administrativeArea)) else None,
      postcode = blpu.postcode,
      uprn = blpu.uprn
    )
  }

  def details(addressWrapper: AddressBaseWrapper, filename: String) = Details(
    blpuCreatedAt = addressWrapper.blpu.startDate.getMillis,
    blpuUpdatedAt = addressWrapper.blpu.lastUpdated.getMillis,
    classification = addressWrapper.classification.classificationCode,
    status = addressWrapper.blpu.blpuState.map(pp => pp.toString),
    state = addressWrapper.blpu.logicalState.map(pp => pp.toString),
    isPostalAddress = addressWrapper.blpu.canReceivePost,
    isResidential = addressWrapper.classification.isResidential,
    isCommercial = !addressWrapper.classification.isResidential,
    usrn = addressWrapper.lpi.usrn,
    file = filename,
    organisation = toSentenceCase(addressWrapper.organisation.map(org => org.organistation)),
    primaryClassification = addressWrapper.classification.primaryUse,
    secondaryClassification = addressWrapper.classification.secondaryUse
  )

}

/**
 * Various utilities for string / field conversions
 */
object formatters {

  def constructPropertyFrom(lpi: LPI) = {
    val formatted = List(formatStartAndEndNumbersAndSuffixes(lpi.saoStartNumber, lpi.saoStartSuffix, lpi.saoEndNumber, lpi.saoEndSuffix), lpi.saoText, lpi.paoText).flatten.mkString(" ")
    if (formatted.isEmpty) None
    else Some(formatted)
  }

  def constructStreetAddressFrom(lpi: LPI, street: StreetWithDescription) =
    if (!street.recordType.isDefined)
      None
    else if (StreetRecordTypeCode.isUnofficialStreet(street.recordType.get))
      None
    else if (StreetRecordTypeCode.isDescription(street.recordType.get))
      None
    else
      Some(String.format("%s %s", constructStreetAddressPrefixFrom(lpi).getOrElse(""), toSentenceCase(street.streetDescription).get).trim)


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
