package uk.gov.gds.model

import scalax.io.LongTraversable
import uk.gov.gds.io._
import scala.collection._
import uk.gov.gds.logging.Logging
import uk.gov.gds.model.AddressBuilder._
import scala.Some

object Transformers extends Logging {

  def processRows(lines: LongTraversable[String])(implicit errors: mutable.MutableList[String], fileName: String) = lines.flatMap(process(_)).toList

  def process(line: String)(implicit errors: mutable.MutableList[String], fileName: String) = {
    val parsed = parseCsvLine(line)

    parsed.head match {
      case BLPU.recordIdentifier =>  extractRow[BLPU](parsed, BLPU)
      case LPI.recordIdentifier => extractRow[LPI](parsed, LPI)
      case Street.recordIdentifier =>   extractRow[Street](parsed, Street)
      case StreetDescriptor.recordIdentifier => extractRow[StreetDescriptor](parsed, StreetDescriptor)
      case Classification.recordIdentifier => extractRow[Classification](parsed, Classification)
      case Organisation.recordIdentifier => extractRow[Organisation](parsed, Organisation)
      case _ => None
    }
  }

  def extractRow[T <: AddressBase](parsed: List[String], addressBase: AddressBaseHelpers[T])(implicit errors: mutable.MutableList[String], fileName: String): Option[T] = {
      if(!addressBase.isValidCsvLine(parsed)) {
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

  def extractStreets(raw: List[AddressBase]) =
    raw flatMap {
      case a: Street => Map(a.usrn -> a)
      case _ => None
    }

  def extractStreetDescriptors(raw: List[AddressBase]) =
    raw flatMap {
      case a: StreetDescriptor => Map(a.usrn -> a)
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

  def constructAddressBaseWrapper(blpus: List[BLPU], lpis: List[LPI]) = {
    val lpisByUprn = lpis.groupBy(_.uprn)

    blpus.map(
      blpu => AddressBaseWrapper(blpu, lpisByUprn.getOrElse(blpu.uprn, List.empty[LPI]))
    ).toList
  }
}
