package uk.gov.gds.model

import scalax.io.LongTraversable
import uk.gov.gds.io._
import uk.gov.gds.logging.Logging
import uk.gov.gds.model.AddressBuilder._
import java.io.File
import scala.Some
import uk.gov.gds.MongoConnection
import uk.gov.gds.logging.Reporter.report
import uk.gov.gds.logging.RowParseError
import uk.gov.gds.logging.MissingLpiError
import uk.gov.gds.logging.MissingClassificationError
import org.joda.time.DateTime


object processors extends Logging {

  import extractors._

  def processStreets(file: File)(implicit mongoConnection: Option[MongoConnection]) = {
    implicit val fileName = file.getName
    logger.info("Processing streets in: " + fileName)

    try {
      persistStreetDescriptors(processRowsIntoStreetsDescriptors(file))
      Some(Result(Success, file.getName))
    } catch {
      case e: Exception => {
        Some(Result(Failure, file.getName))
      }
    }
  }

  def processAddresses(file: File)(implicit mongoConnection: Option[MongoConnection]) = {
    implicit val fileName = file.getName
    logger.info("Processing addresses in: " + fileName)

    try {
      persistAddresses(processRowsIntoAddressWrappers(file))
      Some(Result(Success, fileName))
    } catch {
      case e: Exception => {
        logger.info("Failed to process: [" + file.getName + "]")
        Some(Result(Failure, file.getName))
      }
    }
  }


  def processRowsIntoStreetsDescriptors(file: File) = {

    def processRow(line: String) = {
      val parsed = parseCsvLine(line)
      parsed.head match {
        case StreetDescriptor.recordIdentifier => extractRow[StreetDescriptor](file.getName, parsed, StreetDescriptor)
        case _ => None
      }
    }

    val genericRows = processRows(loadFile(file).lines(), processRow)
    extractStreetDescriptors(genericRows)
  }

  def processRowsIntoAddressWrappers(file: File) = {
    def processRow(line: String) = {
      val parsed = parseCsvLine(line)

      parsed.head match {
        case BLPU.recordIdentifier => extractRow[BLPU](file.getName, parsed, BLPU)
        case LPI.recordIdentifier => extractRow[LPI](file.getName, parsed, LPI)
        case Classification.recordIdentifier => extractRow[Classification](file.getName, parsed, Classification)
        case Organisation.recordIdentifier => extractRow[Organisation](file.getName, parsed, Organisation)
        case _ => None
      }
    }

    val genericRows = processRows(loadFile(file).lines(), processRow)
    extractAddressBaseWrappers(file.getName, genericRows)
  }

  private def processRows(lines: LongTraversable[String], f: String => Option[AddressBase]) = lines.flatMap(f(_)).toList

  private def persistStreetDescriptors(streetDescriptors: List[StreetDescriptor])(implicit mongoConnection: Option[MongoConnection], fileName: String) {
    mongoConnection.foreach(_.insertStreets(streetDescriptors.map(_.serialize)))
  }

  private def persistAddresses(rows: List[AddressBaseWrapper])(implicit mongoConnection: Option[MongoConnection], fileName: String) {
    mongoConnection.foreach(_.insert(rows.flatMap(geographicAddressToSimpleAddress(_)).map(_.serialize)))
  }

}

object extractors {

  implicit object LastUpdatedOrdering extends Ordering[DateTime] {
    def compare(a: DateTime, b: DateTime) = a compareTo b
  }

  /*
    if any file contains an invalid row the we throw an exception and fail the whole file
   */
  def extractRow[T <: AddressBase](fileName: String, parsed: List[String], addressBase: AddressBaseHelpers[T]): Option[T] = {
    if (!addressBase.isValidCsvLine(parsed)) {
      report(fileName, RowParseError, Some(parsed.mkString("|")))
      throw new Exception("Unable to parse row")
    }
    else Some(addressBase.fromCsvLine(parsed))
  }

  def extractBlpus(raw: List[AddressBase]) =
    raw flatMap {
      case a: BLPU => Some(a)
      case _ => None
    }

  def extractLpisByUprn(raw: List[AddressBase]) =
    raw flatMap {
      case a: LPI => Some(a)
      case _ => None
    } groupBy (_.uprn)

  def extractOrganisationsUprn(raw: List[AddressBase]) =
    raw flatMap {
      case a: Organisation => Some(a)
      case _ => None
    } groupBy (_.uprn)

  def extractClassificationsByUprn(raw: List[AddressBase]) =
    raw flatMap {
      case a: Classification => Some(a)
      case _ => None
    } groupBy (_.uprn)

  def extractStreetDescriptors(raw: List[AddressBase]): List[StreetDescriptor] =
    raw flatMap {
      case a: StreetDescriptor => Some(a)
      case _ => None
    }

  def extractAddressBaseWrappers(fileName: String, rows: List[AddressBase]) = {

    val blpus = extractBlpus(rows)
    val lpis = extractLpisByUprn(rows)
    val classifications = extractClassificationsByUprn(rows)
    val organisations = extractOrganisationsUprn(rows)

    blpus.flatMap(
      blpu =>
        buildAddressWrapper(fileName, blpu, lpis, classifications, organisations)
    ).toList
  }

  /*
    Combine the blpu with the lpi and classification into a wrapper. Error if no classification or LPI.
    Errors here don't fail the file - files will fail if some row is invalid - however log the failed UPRN
   */
  def buildAddressWrapper(
                           fileName: String,
                           blpu: BLPU,
                           lpis: Map[String, List[LPI]],
                           classifications: Map[String, List[Classification]],
                           organisations: Map[String, List[Organisation]]) = {

    val lpi = mostRecentLPIForUprn(blpu.uprn, lpis)
    val classification = mostRecentClassificationForUprn(blpu.uprn, classifications)
    val organisation = mostRecentOrganisationForUprn(blpu.uprn, organisations)

    if (!lpi.isDefined) {
      report(fileName, MissingLpiError, Some(blpu.uprn))
      None
    } else if (!classification.isDefined) {
      report(fileName, MissingClassificationError, Some(blpu.uprn))
      None
    } else {
      Some(AddressBaseWrapper(blpu, lpi.get, classification.get, organisation))
    }
  }

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
  def mostRecentClassificationForUprn(uprn: String, classifications: Map[String, List[Classification]]): Option[Classification] =
    classifications.get(uprn) match {
      case Some(classification) => classification.filter(l => !l.endDate.isDefined).sortBy(l => l.lastUpdated).headOption
      case _ => None
    }

  /*
     We want one Organisation per BLPU, and there may be several so remove all with an end date, and get the most recently updated
    */
  def mostRecentOrganisationForUprn(uprn: String, organisations: Map[String, List[Organisation]]): Option[Organisation] =
    organisations.get(uprn) match {
      case Some(organisation) => organisation.filter(l => !l.endDate.isDefined).sortBy(l => l.lastUpdated).headOption
      case _ => None
    }


}
