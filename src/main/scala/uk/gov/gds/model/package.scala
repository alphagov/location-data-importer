package uk.gov.gds

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

package object model {

  sealed abstract class Outcome(r: Boolean)

  case object Success extends Outcome(true)

  case object Failure extends Outcome(false)

  case class Result(outcome: Outcome, message: String)

  implicit def stringToInt(s: String) = java.lang.Integer.parseInt(s)

  implicit def stringToDouble(s: String) = java.lang.Double.parseDouble(s)

  implicit def stringToDate(dateString: String): DateTime = DateTimeFormat.forPattern("yyyy-MM-dd").parseDateTime(dateString)

  implicit def stringToOptionalDate(dateString: String) =
    dateString match {
      case d if !d.isEmpty => Some(stringToDate(d))
      case _ => None
  }

}
