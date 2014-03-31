package uk.gov.gds.location.importer

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

/**
 * Useful implicit conversions for various address base types, doubles, Y/Ns etc
 */
package object model {

  implicit def stringToDouble(s: String) = java.lang.Double.parseDouble(s)

  implicit def yOrNToOptionalBoolean(s: String) = if (s == null || s.isEmpty) None else Some(s.equalsIgnoreCase("Y"))

  implicit def stringToDate(dateString: String): DateTime = DateTimeFormat.forPattern("yyyy-MM-dd").parseDateTime(dateString)

  implicit def stringToOptionalString(s: String) = if (s == null || s.isEmpty) None else Some(s)

  implicit def stringToOptionalDate(dateString: String) =
    dateString match {
      case d if !d.isEmpty => Some(stringToDate(d))
      case _ => None
    }
}
