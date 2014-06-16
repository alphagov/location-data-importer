package uk.gov.gds.location.importer.processors

import uk.gov.gds.location.importer.logging.Logging
import java.io.File
import uk.gov.gds.location.importer.io.FileUtilities._
import scalax.io.{LongTraversable, LineTraversable}
import uk.gov.gds.location.importer.conversions.AddressBaseToLocateConvertor
import uk.gov.gds.location.importer.model._
import scala.Some
import org.joda.time.DateTime

/**
 * Utility methods to convert CSV rows into the model objects required by to create locate style addresses
 */
object AddressBaseRowProcessor extends Logging {

  import AddressBaseToLocateConvertor._

  /**
   * Processes a code point file into a list of CodePoint model objects
   * @param LongTraversable[String] iterator of filelines
   * @return List[CodePoint]
   */
  def processRowsIntoCodePoint(lines: LongTraversable[String], fileName: String) = {

    def processRow(line: String) = {
      val parsed = parseCsvLine(line)
      CodePoint.isValidCsvLine(parsed) match {
        case true => Some(CodePoint.fromCsvLine(parsed))
        case _ => {
          logger.error(String.format("Invalid codepoint row FILENAME [%s] DATA [%s]", fileName, parsed.mkString("|")))
          None
        }
      }
    }

    lines.flatMap(processRow).toList
  }

  /**
   * Processes an address base file to link all the Streets and StreetDescriptors together into a single, wrapper, object
   * Each CSV row is parsed and if relevant an Object of extending AddressBase is created
   * A list of AddressBase objects is returned from the row processing phase
   * These are subsequently converted into a rich data object once the whole file is processed
   * @param LongTraversable[String] iterator of filelines
   * @return List[StreetWrapper]
   */
  def processRowsIntoStreets(lines: LongTraversable[String], fileName: String) = {

    def processRow(line: String) = {

      val parsed = parseCsvLine(line)
      parsed.head match {
        case StreetDescriptor.recordIdentifier => extractRow[StreetDescriptor](fileName, parsed, StreetDescriptor)
        case Street.recordIdentifier => extractRow[Street](fileName, parsed, Street)
        case _ => None
      }
    }

    val genericRows = processRows(lines, processRow)
    extractStreetWrappers(fileName, genericRows)
  }

  /**
   * Processes an address base file
   * Each CSV row is parsed and if relevant an Object of extending AddressBase is created
   * A list of AddressBase objects is returned from the row processing phase
   * These are subsequently converted into a rich data object once the whole file is processed
   * linking all the BLPU, LPI, Organisation and classification rows into AddressWrapper objects
   * @param LongTraversable[String] iterator of filelines
   * @return List[AddressWrapper]
   */
  def processRowsIntoAddressWrappers(lines: LongTraversable[String], fileName: String) = {
    def processRow(line: String) = {
      val parsed = parseCsvLine(line)

      parsed.head match {
        case BLPU.recordIdentifier => extractRow[BLPU](fileName, parsed, BLPU)
        case LPI.recordIdentifier => extractRow[LPI](fileName, parsed, LPI)
        case Classification.recordIdentifier => extractRow[Classification](fileName, parsed, Classification)
        case Organisation.recordIdentifier => extractRow[Organisation](fileName, parsed, Organisation)
        case DeliveryPoint.recordIdentifier => extractRow[DeliveryPoint](fileName, parsed, DeliveryPoint)
        case _ => None
      }
    }

    val genericRows = processRows(lines, processRow)
    extractAddressBaseWrappers(fileName, genericRows)
  }

  private def processRows(lines: LongTraversable[String], f: String => Option[AddressBase]) = lines.flatMap(f(_)).toList

  /**
   * For each row check run a function over it to see if it is:
   * (a) Of a type we are interested in
   * (b) is valid for that type
   * Return an option of that type
   *
   * In event of error fail whole file
   *
   * @param fileName
   * @param parsedCsvLine
   * @param addressBase < AddressBaseHelper
   * @return Option[T < AddressBase]
   */
  private def extractRow[T <: AddressBase](fileName: String, parsedCsvLine: List[String], addressBase: AddressBaseHelpers[T]): Option[T] = {
    if (!addressBase.isValidCsvLine(parsedCsvLine)) {
      logger.error(String.format("Invalid row TYPE [%s] FILENAME [%s] DATA [%s]", fileName, addressBase.getClass.getName, parsedCsvLine.mkString("|")))
      throw new Exception("Unable to parse row " + Some(parsedCsvLine.mkString("|")))
    }
    else Some(addressBase.fromCsvLine(parsedCsvLine))
  }

  /**
   * Extract a list of only the BLPUs from generic list of Address Base types
   *
   * @param raw List[AddressBase]
   * @return Map[String,List[DeliveryPoint]] UPRN to List of DeliveryPoints
   */
  def extractDeliveryPointsByUprn(raw: List[AddressBase]) =
    raw flatMap {
      case a: DeliveryPoint => Some(a)
      case _ => None

    } groupBy (_.uprn)

  /**
   * Extract a list of only the BLPUs from generic list of Address Base types
   *
   * @param raw List[AddressBase]
   * @return List[BLPU]
   */
  def extractBlpus(raw: List[AddressBase]) =
    raw flatMap {
      case a: BLPU => Some(a)
      case _ => None

    }

  /**
   * Extract a list of only the LPIs from generic list of Address Base types, grouped by UPRN
   * May have an historic record of organisation. Linked by UPRN
   *
   * @param raw List[AddressBase]
   * @return Map[String,List[LPI]] UPRN to List of LPI
   */
  def extractLpisByUprn(raw: List[AddressBase]) =
    raw flatMap {
      case a: LPI => Some(a)
      case _ => None
    } groupBy (_.uprn)

  /**
   * Extract a list of only the Organisations from generic list of Address Base types, grouped by UPRN
   * May have an historic record of organisation. Linked by UPRN
   *
   * @param raw List[AddressBase]
   * @return Map[String,List[Organisation]] UPRN to List of Organisation
   */
  def extractOrganisationsUprn(raw: List[AddressBase]) =
    raw flatMap {
      case a: Organisation => Some(a)
      case _ => None
    } groupBy (_.uprn)

  /**
   * Extract a list of only the Classifications from generic list of Address Base types, grouped by UPRN
   * May have an historic record of classification. Linked by UPRN
   *
   * @param raw List[AddressBase]
   * @return Map[String,List[Classification]] UPRN to List of Classification
   */
  def extractClassificationsByUprn(raw: List[AddressBase]) =
    raw flatMap {
      case a: Classification => Some(a)
      case _ => None
    } groupBy (_.uprn)

  /**
   * Extract a list of only the StreetDescriptors from generic list of Address Base types
   *
   * @param raw List[AddressBase]
   * @return List[StreetDescriptor]
   */
  def extractStreetDescriptors(raw: List[AddressBase]) =
    raw flatMap {
      case a: StreetDescriptor => Some(a)
      case _ => None
    }

  /**
   * Extract a list of only the Streets from generic list of Address Base types, grouped by USRN
   * Could have many streets for each USRN as status may have changed
   *
   * @param raw List[AddressBase]
   * @return List[Street]
   */
  def extractStreetsByUsrn(raw: List[AddressBase]) =
    raw flatMap {
      case a: Street => Some(a)
      case _ => None
    } groupBy (_.usrn)

  /**
   * Extract a list of only the AddressBaseWrappers from generic list of Address Base types
   *
   * @param fileName String
   * @param rows List[AddressBase]
   * @return List[AddressBaseWrapper]
   */
  def extractAddressBaseWrappers(fileName: String, rows: List[AddressBase]) = {

    val blpus = extractBlpus(rows)
    val lpis = extractLpisByUprn(rows)
    val classifications = extractClassificationsByUprn(rows)
    val organisations = extractOrganisationsUprn(rows)
    val deliveryPoints = extractDeliveryPointsByUprn(rows)

    blpus.flatMap(
      blpu =>
        toAddressBaseWrapper(fileName, blpu, lpis, classifications, organisations, deliveryPoints)
    ).toList
  }

  /**
   * Extract a list of only the StreetWrappers from generic list of Address Base types
   *
   * @param fileName String
   * @param rows List[AddressBase]
   * @return List[StreetWrapper]
   */
  def extractStreetWrappers(fileName: String, rows: List[AddressBase]) = {

    val streets = extractStreetsByUsrn(rows)
    val streetDescriptions = extractStreetDescriptors(rows)

    streetDescriptions.flatMap(
      streetDescription =>
        toStreetWithDescription(fileName, streets, streetDescription)
    ).toList
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
                            organisations: Map[String, List[Organisation]],
                            deliveryPoints: Map[String, List[DeliveryPoint]]) = {

    val lpi = mostRecentActiveLPIForUprn(blpu.uprn, lpis)
    val classification = mostRecentActiveClassificationForUprn(blpu.uprn, classifications)
    val organisation = mostRecentActiveOrganisationForUprn(blpu.uprn, organisations)
    val deliveryPoint = mostRecentActiveDeliveryPointForUprn(blpu.uprn, deliveryPoints)

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
      Some(AddressBaseWrapper(updateBlpuPostcodeIfRequired(fileName, blpu, deliveryPoint), lpi.get, classification.get, organisation, deliveryPoint))
    }
  }

  /**
   * Potentially the delivery point postcode may differ from the BLPU
   * If so Delivery Point should override BLPU
   * @param blpu
   * @param deliveryPoint
   * @return
   */
  private def updateBlpuPostcodeIfRequired(fileName: String, blpu: BLPU, deliveryPoint: Option[DeliveryPoint]) =
    deliveryPoint match {
      case Some(dp) if !same(blpu.postcode, dp.postcode) => {
        logger.info("using DeliveryPoint postcode filename [%s] UPRN [%s] BLPU postcode [%s] DeliveryPoint postcode [%s]".format(fileName, blpu.uprn, blpu.postcode, dp.postcode))
        blpu.copy(postcode = dp.postcode)
      }
      case _ => blpu
    }

  private def same(one: String, two: String) = one.replace(" ", "").equalsIgnoreCase(two.replace(" ", ""))

  /*
   BLPU checker - all BLPUs must NOT have an end date - indicates an active property
  */
  def blpuIsActive(blpu: BLPU) = !blpu.endDate.isDefined

  implicit object LastUpdatedOrdering extends Ordering[DateTime] {
    def compare(a: DateTime, b: DateTime) = b compareTo a
  }

  def toStreetWithDescription(fileName: String, streets: Map[String, List[Street]], streetDescriptor: StreetDescriptor) = {
    val street = mostRecentActiveStreetForUsrn(streetDescriptor.usrn, streets)

    if (streets.get(streetDescriptor.usrn).isEmpty) {
      logger.error(String.format("No street found for USRN [%s] file [%s]", streetDescriptor.usrn, fileName))
      None
    } else if (!street.isDefined) {
      logger.error(String.format("No active street found for USRN [%s] file [%s]", streetDescriptor.usrn, fileName))
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
     We want one LPI per BLPU, and there may be several so remove all with an end date, and get the most recently updated
     If more than one use the most recent official one if possible
    */
  def mostRecentActiveLPIForUprn(uprn: String, lpis: Map[String, List[LPI]]): Option[LPI] =
    lpis.get(uprn) match {
      case Some(lpi) => {
        // all active LPIs sorted by last updated
        val lpis = lpi.filter(l => !l.endDate.isDefined).sortBy(l => l.lastUpdated)

        // only one active LPI - return it as an option (none if nothing in list)
        if (lpis.size <= 1) lpis.headOption
        else {
          // if we have official LPIs - return most recent
          lpis.filter(l => l.officialAddress.getOrElse(false)) match {
            case officalLpis if officalLpis.size > 0  => officalLpis.headOption
            case _ => lpis.headOption
          }
        }
      }
      // No LPIs so None
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

  /*
   We want one Delivery Point per UPRN, and there may be several so remove all with an end date, and get the most recently updated
  */
  def mostRecentActiveDeliveryPointForUprn(uprn: String, deliveryPoints: Map[String, List[DeliveryPoint]]): Option[DeliveryPoint] =
    deliveryPoints.get(uprn) match {
      case Some(deliveryPoint) => deliveryPoint.filter(l => !l.endDate.isDefined).sortBy(l => l.lastUpdated).headOption
      case _ => None
    }
}


