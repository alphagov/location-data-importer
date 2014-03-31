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

/**
 * Object to take lists of address base types and convert to our address / street model
 */
object AddressBaseToLocateConvertor extends Logging {

  import formatters._

  def toLocateAddress(addressWrapper: AddressBaseWrapper, fileName: String) = {
    val streetDescriptor = AllTheStreets.allTheStreets.get(addressWrapper.lpi.usrn)

    streetDescriptor match {
      case Some(street) => {

        val codePoint = AllTheCodePoints.codePoints.get(addressWrapper.blpu.postcode.toLowerCase.replaceAll(" ", ""))

        codePoint match {
          case Some(code) => {
            // query LA code against lat long
            //            val boundaryLine = mongo.flatMap(_.boundaryLineForGssCode(codePoint.get._1, addressWrapper.blpu.northing, addressWrapper.blpu.easting))
            //
            //            // This code point object has the correct LA
            //            if (boundaryLine.isDefined) {
            address(addressWrapper, code._2, code._1, street, fileName)
            //            } else {
            //              report(fileName, IncorrectGssCodeFromCodePoint, List(addressWrapper.uprn, addressWrapper.blpu.postcode, code._1))
            //              val boundaryLine = mongo.flatMap(_.boundaryLineForLatLong(addressWrapper.blpu.northing, addressWrapper.blpu.easting))
            //
            //              boundaryLine match {
            //                // Found an LA by geo, use this LA code
            //                case Some(bl) => {
            //                  report(fileName, FoundGssCodeFromBoundaryLine, List(addressWrapper.uprn, addressWrapper.blpu.postcode, code._1, bl.properties.CODE, addressWrapper.blpu.northing.toString, addressWrapper.blpu.easting.toString))
            //                  address(addressWrapper, code._2, bl.properties.CODE, street, fileName)
            //                }
            //                // No LA found at all
            //                case _ => {
            //                  report(fileName, NoGssCodeFromBoundaryLine, List(addressWrapper.uprn, addressWrapper.blpu.postcode, code._1, addressWrapper.blpu.northing.toString, addressWrapper.blpu.easting.toString))
            //                  None
            //                }
            //              }
            //            }

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
    startHouseNumber = addressWrapper.lpi.paoStartNumber.map(n => Integer.valueOf(n)),
    endHouseNumber = addressWrapper.lpi.paoEndNumber.map(n => Integer.valueOf(n)),
    houseName = toSentenceCase(addressWrapper.lpi.paoText)
  )

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
    organisation = toSentenceCase(addressWrapper.organisation.map(org => org.organistation))
  )

  def location(blpu: BLPU) = Location(blpu.easting, blpu.northing)

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


  implicit object LastUpdatedOrdering extends Ordering[DateTime] {
    def compare(a: DateTime, b: DateTime) = b compareTo a
  }


  def toStreetWrapper(fileName: String, streets: Map[String, List[Street]], streetDescriptor: StreetDescriptor) = {
    val street = mostRecentActiveStreetForUsrn(streetDescriptor.usrn, streets)

    if (streets.get(streetDescriptor.usrn).isEmpty) {
      logger.error(String.format("No street found for USRN [%s]", streetDescriptor.usrn, fileName))
      None
    } else if (!street.isDefined) {
      logger.error(String.format("No active street found for USRN [%s]", streetDescriptor.usrn, fileName))
      None
    } else
      street.map(s => StreetWithDescription(
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
      ))
  }

  /*
    Combine the blpu with the lpi and classification into a wrapper. Error if no classification or LPI.
    Errors here don't fail the file - files will fail if some row is invalid - however log the failed UPRN
   */
  def toAddressBaseWrapper(
                            fileName: String,
                            blpu: BLPU,
                            lpis: Map[String, List[LPI]],
                            classifications: Map[String, List[Classification]],
                            organisations: Map[String, List[Organisation]]) = {

    val lpi = mostRecentLPIForUprn(blpu.uprn, lpis)
    val classification = mostRecentActiveClassificationForUprn(blpu.uprn, classifications)
    val organisation = mostRecentActiveOrganisationForUprn(blpu.uprn, organisations)

    if (!blpuIsActive(blpu)) {
      logger.error(String.format("BLPU is inactive BLPU [%s] POSTCODE [%s] FILENAME [%s]", blpu.uprn, blpu.postcode, fileName))
      None
    } else if (lpis.getOrElse(blpu.uprn, List.empty).isEmpty) {
      logger.error(String.format("BLPU has no matching LPI [%s] POSTCODE [%s] FILENAME [%s]", blpu.uprn, blpu.postcode, fileName))
      None
    } else if (!lpi.isDefined) {
      logger.error(String.format("BLPU has no matching active LPI [%s] POSTCODE [%s] FILENAME [%s]", blpu.uprn, blpu.postcode, fileName))
      None
    } else if (!classification.isDefined) {
      logger.error(String.format("BLPU has no classification UPRN [%s] POSTCODE [%s] FILENAME [%s]", blpu.uprn, blpu.postcode, fileName))
      None
    } else {
      Some(AddressBaseWrapper(blpu, lpi.get, classification.get, organisation))
    }
  }

  /*
   BLPU checker - all BLPUs must NOT have an end date - indicates an active property
  */
  def blpuIsActive(blpu: BLPU) = !blpu.endDate.isDefined


  /*
     We want one LPI per BLPU, and there may be several so remove all with an end date, and get the most recently updated
    */
  def mostRecentLPIForUprn(uprn: String, lpis: Map[String, List[LPI]]): Option[LPI] =
    lpis.get(uprn) match {
      case Some(lpi) => lpi.filter(l => !l.endDate.isDefined).sortBy(l => l.lastUpdated).headOption
      case _ => None
    }

  /*
     We want one Classification per BLPU, and there may be several so remove all with an end date, and get the most recently updated
    */
  def mostRecentActiveClassificationForUprn(uprn: String, classifications: Map[String, List[Classification]]): Option[Classification] =
    classifications.get(uprn) match {
      case Some(classification) => classification.filter(l => !l.endDate.isDefined).sortBy(l => l.lastUpdated).headOption
      case _ => None
    }

  /*
     We want one Organisation per BLPU, and there may be several so remove all with an end date, and get the most recently updated
    */
  def mostRecentActiveOrganisationForUprn(uprn: String, organisations: Map[String, List[Organisation]]): Option[Organisation] =
    organisations.get(uprn) match {
      case Some(organisation) => organisation.filter(l => !l.endDate.isDefined).sortBy(l => l.lastUpdated).headOption
      case _ => None
    }

  /*
    We want one Street per USRN, and there may be several so remove all with an end date, and get the most recently updated
   */
  def mostRecentActiveStreetForUsrn(usrn: String, streets: Map[String, List[Street]]): Option[Street] =
    streets.get(usrn) match {
      case Some(street) => street.filter(l => !l.endDate.isDefined).sortBy(l => l.lastUpdated).headOption
      case _ => None
    }
}

object formatters {

  def toSentenceCase(field: Option[String]) = field.map(f => (f toLowerCase) split (" ") map (_.capitalize) mkString (" "))

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
