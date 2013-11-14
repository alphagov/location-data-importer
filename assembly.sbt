import AssemblyKeys._

assemblySettings

jarName in assembly := "location-data-importer.jar"

test in assembly := {}

mainClass in assembly := Some("uk.gov.gds.LocationDataImporter")




