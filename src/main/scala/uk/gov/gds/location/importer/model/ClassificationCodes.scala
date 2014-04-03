package uk.gov.gds.location.importer.model

object ClassificationCodes {



  def primaryCodeFor(classification: String) = {
    if (classification.length <= 0) None
    else primaryCodes.get(classification.substring(0, 1))
  }

  def secondaryCodeFor(classification: String) = {
    if (classification.length < 2) None
    else secondaryCodes.get(classification.substring(0, 2))
  }

  def isResidential(classification: String) =
    classification.toUpperCase.startsWith("RD") || // Dwelling
      classification.toUpperCase.startsWith("RH") || // Multiple Occupance
      classification.toUpperCase.startsWith("RI") // Institute (nursing home etc)

  def isCommercial(classification: String) = classification.toUpperCase.startsWith("C")

  private val primaryCodes: Map[String, String] = Map(
    "C" -> "Commercial",
    "L" -> "Land",
    "M" -> "Military",
    "O" -> "Other",
    "P" -> "Parent Shell",
    "R" -> "Residential",
    "U" -> "Unclassified",
    "X" -> "Dual Use",
    "Z" -> "Object of interest"
  )

  private val secondaryCodes = Map(
    "CA" -> "Agriculture",
    "CB" -> "Ancillary Building",
    "CC" -> "Community Services",
    "CE" -> "Education",
    "CH" -> "Hotel / Motel / Boarding / Guest House",
    "CI" -> "Industrial Application",
    "CL" -> "Lesiure",
    "CM" -> "Medical",
    "CN" -> "Animal Centre",
    "CO" -> "Office",
    "CR" -> "Retail",
    "CS" -> "Storage",
    "CT" -> "Transport",
    "CU" -> "Utility",
    "CX" -> "Emergency Services",
    "CZ" -> "Information",
    "LA" -> "Agricultural Land",
    "LB" -> "Ancillary Building",
    "LC" -> "Burial Ground",
    "LD" -> "Development",
    "LF" -> "Forestry",
    "LL" -> "Allotment",
    "LM" -> "Amenity",
    "LO" -> "Open Space",
    "LP" -> "Park",
    "LU" -> "Unused Land",
    "LW" -> "Water",
    "MA" -> "Army",
    "MB" -> "Ancillary Building",
    "MF" -> "Air Force",
    "MG" -> "Defense Estates",
    "MN" -> "Navy",
    "OA" -> "Aid To Naviation",
    "OC" -> "Coastal Protection / Flood Prevention",
    "OE" -> "Emergency Support",
    "OF" -> "Street Furniture",
    "OG" -> "Agricultural Support Objects",
    "OH" -> "Historical Site",
    "OI" -> "Industrial Support",
    "ON" -> "Significant Natural Object",
    "OO" -> "Ornamental / Cultural Object",
    "OP" -> "Sport / Leisure Support",
    "OR" -> "Royal Mail Infrastructure",
    "OS" -> "Scientific / Observation Support",
    "OT" -> "Transport Support",
    "PP" -> "Property Shell",
    "PS" -> "Street Record",
    "RB" -> "Ancillary Building",
    "RC" -> "Car Park Space",
    "RD" -> "Dwelling",
    "RG" -> "Garage",
    "RH" -> "House In Multiple Occupation",
    "RI" -> "Residential Institution",
    "UC" -> "Awaiting Classification",
    "UP" -> "Pending Internal Investigation",
    "ZA" -> "Archaeological Dig Site",
    "ZM" -> "Monument",
    "ZS" -> "Stately Home",
    "ZU" -> "Underground Feature",
    "ZW" -> "Place Of Worship"
  )
}
