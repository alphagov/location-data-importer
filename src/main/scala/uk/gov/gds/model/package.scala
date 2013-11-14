package uk.gov.gds

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

package object model {
  implicit def stringToInt(s: String) = java.lang.Integer.parseInt(s)

  implicit def stringToDouble(s: String) = java.lang.Double.parseDouble(s)

  implicit def stringToDate(dateString: String): DateTime = DateTimeFormat.forPattern("yyyy-MM-dd").parseDateTime(dateString)

  implicit def stringToOptionalDate(dateString: String) =
    dateString match {
      case d if !d.isEmpty => Some(stringToDate(d))
      case _ => None
  }

}
