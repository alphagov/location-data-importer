package uk.gov.gds.model

import scalax.io.LongTraversable
import uk.gov.gds.io._
import scala.Some
import scala.collection._
import uk.gov.gds.logging.Logging

object Transformers extends Logging {

  def processRows(lines: LongTraversable[String])(implicit errors: mutable.MutableList[String], fileName: String) = {
    logger.info("processing: " + fileName)
    lines.flatMap(process(_)).toList
  }

  def process(line: String)(implicit errors: mutable.MutableList[String], fileName: String) = {
    val parsed = parseCsvLine(line)

    parsed(0) match {
      case BLPU.recordIdentifier =>  extractRow[BLPU](parsed, BLPU)
      case LPI.recordIdentifier => extractRow[LPI](parsed, LPI)
      case Street.recordIdentifier =>   extractRow[Street](parsed, Street)
      case StreetDescriptor.recordIdentifier => extractRow[StreetDescriptor](parsed, StreetDescriptor)
      case _ => None
    }
  }

  def extractRow[T <: AddressBase](parsed: List[String], addressBase: AddressBaseHelpers[T])(implicit errors: mutable.MutableList[String], fileName: String): Option[T] = {
      if(!addressBase.isValidCsvLine(parsed)) {
        errors += "ROW Error filename [" + fileName + "] row [" + parsed.mkString(", ") + "]"
        None
      }
      else Some(addressBase.fromCsvLine(parsed))
  }

  def extractBlpu(raw: List[AddressBase]) =
    raw flatMap {
      case a: BLPU => Some(a)
      case _ => None
    }

  def extractLpi(raw: List[AddressBase]) =
    raw flatMap {
      case a: LPI => Some(a)
      case _ => None
    }

  def constructAddressBaseWrapper(blpus: List[BLPU], lpis: List[LPI]) = {
    val lpisByUprn = lpis.groupBy(_.uprn)

    blpus.map(
      blpu => blpu.uprn -> AddressBaseWrapper(blpu, lpisByUprn.getOrElse(blpu.uprn, List.empty[LPI]))
    ).toMap
  }
}
