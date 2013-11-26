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

object transformers extends Logging {

  def processStreets(file: File)(implicit mongoConnection: Option[MongoConnection]) = {
    logger.info("Processing streets " + file.getName)

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
    logger.info("Processing " + fileName)

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
    constructAddressBaseWrappers(genericRows)
  }

  private def processRows(lines: LongTraversable[String], f: String => Option[AddressBase]) = lines.flatMap(f(_)).toList

  def persistStreetDescriptors(streetDescriptors: List[StreetDescriptor])(implicit mongoConnection: Option[MongoConnection]) {
    mongoConnection.foreach(_.insertStreets(streetDescriptors.map(_.serialize)))
  }

  def persistAddresses(rows: List[AddressBaseWrapper])(implicit mongoConnection: Option[MongoConnection]) {
    mongoConnection.foreach(_.insert(rows.flatMap(geographicAddressToSimpleAddress(_)).map(_.serialize)))
  }

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

  def extractLpis(raw: List[AddressBase]) =
    raw flatMap {
      case a: LPI => Some(a)
      case _ => None
    }

  def extractStreetDescriptors(raw: List[AddressBase]): List[StreetDescriptor] =
    raw flatMap {
      case a: StreetDescriptor => Some(a)
      case _ => None
    }

  def extractOrganisations(raw: List[AddressBase]) =
    raw flatMap {
      case a: Organisation => Some(a)
      case _ => None
    }

  def extractClassifications(raw: List[AddressBase]) =
    raw flatMap {
      case a: Classification => Some(a)
      case _ => None
    }

  def constructAddressBaseWrappers(rows: List[AddressBase]) = {

    val blpus = extractBlpus(rows)
    val lpis = extractLpis(rows)
    val classifications = extractClassifications(rows)
    val organisations = extractOrganisations(rows)

    val lpisByUprn = lpis.groupBy(_.uprn)
    val classificationsByUprn = classifications.groupBy(_.uprn)
    val organisationsByUprn = organisations.groupBy(_.uprn)

    blpus.map(
      blpu =>
        AddressBaseWrapper(
          blpu,
          lpisByUprn.getOrElse(blpu.uprn, List.empty[LPI]),
          classificationsByUprn.getOrElse(blpu.uprn, List.empty[Classification]),
          organisationsByUprn.getOrElse(blpu.uprn, List.empty[Organisation])
        )
    ).toList
  }
}
