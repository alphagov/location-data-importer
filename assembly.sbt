import AssemblyKeys._

assemblySettings

jarName in assembly := "location-data-importer.jar"

test in assembly := {}

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case "index.html"     => MergeStrategy.discard
    case x => old(x)
  }
}

mainClass in assembly := Some("uk.gov.gds.LocationDataImporter")




