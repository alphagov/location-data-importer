package uk.gov.gds.location.importer.model

/**
 * ONS Country codes
 */
object Countries {
  val countries = Map(
    "S92000003" -> "Scotland",
    "E92000001" -> "England",
    "W92000004" -> "Wales"
  )

  def countryForGssCode(gssCode: String) = gssCode.substring(0, 1) match {
    case "E" => "England"
    case "S" => "Scotland"
    case "W" => "Wales"
  }
}
