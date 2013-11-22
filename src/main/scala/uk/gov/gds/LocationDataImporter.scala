package uk.gov.gds

import scopt.OptionParser
import uk.gov.gds.io.ProcessAddressBaseFiles
import uk.gov.gds.logging.Logging
import uk.gov.gds.io.{Failure, Success}

object LocationDataImporter extends Logging {

  case class Config(dir: String = "", persist: Boolean = true)

  def main(args: Array[String]) {

    val opts = new OptionParser[Config]("Location Data Importer") {
      head("Parse and import location data", "0.1")
      opt[String]('d', "dir") text "Location of address base files files" required() action {
        (dir: String, c: Config) => c.copy(dir = dir)
      }
      opt[Boolean]('p', "persist") text "Persist the data" required() action {
        (p: Boolean, c: Config) => c.copy(persist = p)
      }
      help("help") text "use -d or -dir to identify source directory containg files to parse"
      version("version") text "0.1"
    }

    opts.parse(args, Config()) map {
      config => {
        logger.info("Processing: " + config.dir + " Persisting: " + config.persist)

        implicit val mongoConnection = config.persist match {
          case true => Some(new MongoConnection)
          case false => None
        }

        val result = ProcessAddressBaseFiles.process(config.dir)

        result.outcome match {
          case Success => logger.info("Completed processing: \n" + result.messages.mkString("\n"))
          case Failure => logger.info("Failed processing: \n" + result.messages.mkString("\n"))
          case _ => logger.info("Failed processing: Unable to generate a result]")
        }
      }
    }
  }

}
