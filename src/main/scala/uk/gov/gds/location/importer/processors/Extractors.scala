package uk.gov.gds.location.importer.processors

import uk.gov.gds.location.importer.logging.Logging
import java.io.File
import uk.gov.gds.location.importer.io.FileUtilities._
import scalax.io.{LineTraversable, LongTraversable}
import uk.gov.gds.location.importer.conversions.AddressBaseToLocateConvertor
import uk.gov.gds.location.importer.model._
import scala.Some

/**
 * Utility methods to convert CSV rows into the model objects required by to create locate style addresses
 */
object Extractors extends Logging {

  import AddressBaseToLocateConvertor._

  /**
   * Processes a code point file into a list of CodePoint model objects
   * @param file
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
   * @param file
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
   * Processes an address base file, linking all the BLPU, LPI, Organisation and classification rows into AddressWrapper objects
   * @param file
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
  def extractRow[T <: AddressBase](fileName: String, parsedCsvLine: List[String], addressBase: AddressBaseHelpers[T]): Option[T] = {
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
   * @return List[LPI]
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
   * @return List[Organisation]
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
   * @return List[Classification]
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

    blpus.flatMap(
      blpu =>
        toAddressBaseWrapper(fileName, blpu, lpis, classifications, organisations)
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
        toStreetWrapper(fileName, streets, streetDescription)
    ).toList
  }
}


