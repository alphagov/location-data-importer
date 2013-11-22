package uk.gov.gds.model

import scalax.io.LongTraversable
import uk.gov.gds.io._
import scala.collection.mutable.MutableList
import scala.collection.immutable.Map
import uk.gov.gds.logging.Logging
import uk.gov.gds.model.AddressBuilder._
import java.io.File
import scala.Some
import uk.gov.gds.MongoConnection

object Transformers extends Logging {

  def processFile(file: File)(implicit mongoConnection: Option[MongoConnection]) = {
    logger.info("Processing " + file.getName)

    val errors = MutableList.empty[String]
    val rows = processRows(loadFile(file).lines())(errors, file.getName)
    if (errors.isEmpty) {

      val blpus = extractBlpus(rows)
      val lpis = extractLpis(rows)
      val streets = extractStreets(rows)
      val classifications = extractClassifications(rows)
      val organisations = extractOrganisations(rows)
      val streetDescriptors = extractStreetDescriptors(rows)

      val addressBase = constructAddressBaseWrapper(blpus, lpis, classifications, organisations)

      mongoConnection.foreach(_.insert(addressBase.flatMap(geographicAddressToSimpleAddress(_, streets, streetDescriptors)).map(_.serialize)))

      Some(Result(Success, file.getName))
    } else {
      logger.info(errors.mkString("\n"))
      Some(Result(Failure, file.getName))
    }

  }

  def processRows(lines: LongTraversable[String])(implicit errors: MutableList[String], fileName: String) = lines.flatMap(process(_)).toList

  def process(line: String)(implicit errors: MutableList[String], fileName: String) = {
    val parsed = parseCsvLine(line)

    parsed.head match {
      case BLPU.recordIdentifier => extractRow[BLPU](parsed, BLPU)
      case LPI.recordIdentifier => extractRow[LPI](parsed, LPI)
      case Street.recordIdentifier => extractRow[Street](parsed, Street)
      case StreetDescriptor.recordIdentifier => extractRow[StreetDescriptor](parsed, StreetDescriptor)
      case Classification.recordIdentifier => extractRow[Classification](parsed, Classification)
      case Organisation.recordIdentifier => extractRow[Organisation](parsed, Organisation)
      case _ => None
    }
  }

  def extractRow[T <: AddressBase](parsed: List[String], addressBase: AddressBaseHelpers[T])(implicit errors: MutableList[String], fileName: String): Option[T] = {
    if (!addressBase.isValidCsvLine(parsed)) {
      errors += "Row error for filename=[" + fileName + "] row data=[" + parsed.mkString(", ") + "]"
      None
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

  def extractStreets(raw: List[AddressBase]): Map[String, List[Street]] = {
    raw flatMap {
      case a: Street => Some(a)
      case _ => None
    } groupBy (_.usrn)
  }

  def extractStreetDescriptors(raw: List[AddressBase]): Map[String, StreetDescriptor] = {
    val sd = raw flatMap {
      case a: StreetDescriptor => Some(a)
      case _ => None
    }
    sd.groupBy(_.usrn).map(a => a._1 -> a._2.head)
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

  def constructAddressBaseWrapper(blpus: List[BLPU], lpis: List[LPI], classifications: List[Classification] = List.empty[Classification], organisations: List[Organisation] = List.empty[Organisation]) = {
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
