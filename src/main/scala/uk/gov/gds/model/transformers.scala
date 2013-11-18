package uk.gov.gds.model

import scalax.io.LongTraversable
import uk.gov.gds.io._
import scala.Some

object Transformers {

  def processRows(lines: LongTraversable[String]) = lines.flatMap(line => {
    if (line.startsWith(BLPU.recordIdentifier)) Some(BLPU.fromCsvLine(parseCsvLine(line)))
    else if (line.startsWith(LPI.recordIdentifier)) Some(LPI.fromCsvLine(parseCsvLine(line)))
    else if (line.startsWith(Street.recordIdentifier)) Some(Street.fromCsvLine(parseCsvLine(line)))
    else if (line.startsWith(StreetDescriptor.recordIdentifier)) Some(StreetDescriptor.fromCsvLine(parseCsvLine(line)))
    else None
  }).toList

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
