package uk.gov.gds.model

import uk.gov.gds.logging.Logging
import uk.gov.gds.MongoConnection
import uk.gov.gds.logging.Reporter.report
import uk.gov.gds.logging.NoStreetForBlpuError
import uk.gov.gds.logging.NoCodePointForPostcode


object AddressBuilder extends Logging {

  import formatters._

  def geographicAddressToSimpleAddress(addressWrapper: AddressBaseWrapper)(implicit mongo: Option[MongoConnection], fileName: String) = {
    val streetDescriptor: Option[StreetWithDescription] = mongo.flatMap(_.streetForUsrn(addressWrapper.lpi.usrn))


    streetDescriptor match {
      case Some(street) => {
        val codePoint: Option[CodePoint] = mongo.flatMap(_.codePointForPostcode(addressWrapper.blpu.postcode))

        codePoint match {
          case Some(code) =>
            Some(Address(
              houseName = addressWrapper.lpi.paoText,
              houseNumber = constructStreetAddressPrefixFrom(addressWrapper.lpi),
              gssCode = code.district,
              countryCode = code.country,
              postcode = addressWrapper.blpu.postcode.toLowerCase.replaceAll(" ", ""),
              presentation = presentation(addressWrapper.blpu, addressWrapper.lpi, street),
              location = location(addressWrapper.blpu),
              details = details(addressWrapper, fileName)
            ))
          case _ => {
            report(fileName, NoCodePointForPostcode, addressWrapper.uprn)
            None
          }
        }
      }
      case _ => {
        report(fileName, NoStreetForBlpuError, addressWrapper.uprn)
        None
      }

    }
  }

  /*
    Model class builders
   */
  def details(addressWrapper: AddressBaseWrapper, filename: String) = Details(
    blpuCreatedAt = addressWrapper.blpu.startDate,
    blpuUpdatedAt = addressWrapper.blpu.lastUpdated,
    classification = addressWrapper.classification.classificationCode,
    status = addressWrapper.blpu.blpuState.map(pp => pp.toString),
    state = addressWrapper.blpu.logicalState.map(pp => pp.toString),
    isPostalAddress = addressWrapper.blpu.canReceivePost,
    isResidential = addressWrapper.classification.isResidential,
    isCommercial = !addressWrapper.classification.isResidential,
    usrn = addressWrapper.lpi.usrn,
    file = filename,
    organisation = addressWrapper.organisation.map(org => org.organistation)
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
