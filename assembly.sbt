import AssemblyKeys._
import sbtassembly.Plugin.{PathList, MergeStrategy}

assemblySettings

jarName in assembly := "location-data-importer.jar"

test in assembly := {}

mergeStrategy in assembly <<= (mergeStrategy in assembly) {
  (old) => {
    case "index.html" => MergeStrategy.discard
    case "plugin.properties" => MergeStrategy.discard
    case "plugin.xml" => MergeStrategy.discard
    case "about.html" => MergeStrategy.discard
    case PathList("javax", "xml", xs @_ *) => MergeStrategy.first
    case PathList("org", "apache", "commons", xs @_ *) => MergeStrategy.first
    case PathList("org", "apache", "xmlcommons", xs @_ *) => MergeStrategy.first
    case x => old(x)
  }
}

mainClass in assembly := Some("uk.gov.gds.LocationDataImporter")




