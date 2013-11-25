package uk.gov.gds

import scopt.OptionParser
import uk.gov.gds.io.ProcessAddressBaseFiles
import uk.gov.gds.logging.{Reporter, Logging}
import uk.gov.gds.io.{Failure, Success}

object LocationDataImporter extends Logging {

  case class Config(file: String = "", dir: String = "", persist: Boolean = false, index: Boolean = false, username: String = "", password: String = "")

  def main(args: Array[String]) {

    val opts = new OptionParser[Config]("Location Data Importer") {
      head("Parse and import location data", "0.1")
      opt[String]('d', "dir") text "Location of address base files files" action {
        (dir: String, c: Config) => c.copy(dir = dir)
      }
      opt[String]('f', "file") text "Specific file (full path required) to process" action {
        (file: String, c: Config) => c.copy(file = file)
      }
      opt[Unit]('p', "persist") text "Persist the data" action {
        (_, c: Config) => c.copy(persist = true)
      }
      opt[Unit]('i', "index") text "Index the mongo" action {
        (_, c: Config) => c.copy(index = true)
      }
      opt[String]('u', "username") text "Username for the mongo" action {
        (p: String, c: Config) => c.copy(username = p)
      }
      opt[String]('p', "password") text "Password for the mongo" action {
        (p: String, c: Config) => c.copy(password = p)
      }
      help("help") text "use -d or -dir to identify source directory containing files to parse"
      version("version") text "0.1"
    }

    opts.parse(args, Config()) map {
      config => {
        logger.info("Processing: " + config.dir + " Persisting: " + config.persist)

        implicit val mongoConnection = config.persist match {
          case true if !config.username.isEmpty && !config.password.isEmpty => Some(new MongoConnection(Some(config.username), Some(config.password)))
          case true => Some(new MongoConnection)
          case false => None
        }

        val result = if(!config.file.isEmpty) ProcessAddressBaseFiles.processSingleFile(config.file)
        else ProcessAddressBaseFiles.process(config.dir)

        if(config.index) {
          logger.info("adding indexes")
          mongoConnection.foreach(_.addIndexes())
        }

        result.outcome match {
          case Success => logger.info("Completed processing: \n" + result.messages.mkString("\n"))
          case Failure => logger.info("Failed processing: \n" + result.messages.mkString("\n"))
          case _ => logger.info("Failed processing: Unable to generate a result]")
        }
      }
    }
  }

}
