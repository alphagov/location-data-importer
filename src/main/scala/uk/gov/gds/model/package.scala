package uk.gov.gds

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

package object model {

  // TODO fix this
  implicit def stringToInt(s: String) = try {
    java.lang.Integer.parseInt(s)
  } catch {
    case e: Exception => 9999
  }

  // TODO and this
  implicit def stringToDouble(s: String) = try {
    java.lang.Double.parseDouble(s)
  } catch {
    case e: Exception => 9999
  }

  implicit def yesNoToOptionalBoolean(s: String) = if (s == null || s.isEmpty) None else Some(s.equalsIgnoreCase("Y"))

  implicit def stringToDate(dateString: String): DateTime = DateTimeFormat.forPattern("yyyy-MM-dd").parseDateTime(dateString)

  implicit def stringToOptionalString(s: String) = if (s == null || s.isEmpty) None else Some(s)

  implicit def stringToOptionalDate(dateString: String) =
    dateString match {
      case d if !d.isEmpty => Some(stringToDate(d))
      case _ => None
    }
}
