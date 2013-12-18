package uk.gov.gds

import scopt.OptionParser
import uk.gov.gds.io.ProcessAddressBaseFiles
import uk.gov.gds.logging.{Reporter, Logging}
import uk.gov.gds.io.{Failure, Success}
import java.io.File
import com.mongodb.casbah.commons.conversions.scala.RegisterJodaTimeConversionHelpers
import org.joda.time.DateTime

object LocationDataImporter extends Logging {

  case class Config(dir: String = "", codePoint: String = "", cleanReport: Boolean = false, username: String = "", password: String = "")

  def main(args: Array[String]) {

    RegisterJodaTimeConversionHelpers()

    val opts = new OptionParser[Config]("Location Data Importer") {
      head("Parse and import location data", "0.1")
      opt[String]('a', "addresses") required() text "Location of address base files files" action {
        (dir: String, c: Config) => c.copy(dir = dir)
      }
      opt[String]('c', "codepoint") required() text "Location of code point files)" action {
        (file: String, c: Config) => c.copy(codePoint = file)
      }
      opt[Unit]('r', "removeReport") text "Remove the reports. (Default don't)" action {
        (_, c: Config) => c.copy(cleanReport = true)
      }
      opt[String]('p', "password") text "Password for the mongo (default none)" action {
        (p: String, c: Config) => c.copy(password = p)
      }
      opt[String]('u', "username") text "Username for the mongo (default none)" action {
        (p: String, c: Config) => c.copy(username = p)
      }
      help("help") text "use -d or -dir to identify source directory containing files to parse"
      version("version") text "0.1"
    }

    opts.parse(args, Config()) map {
      config => {
        val start = new DateTime

        if (config.cleanReport) {
          new File(Reporter.reportFile).delete()
          new File(Reporter.processed).delete()
        }

        /*
          Initialize the mongo connection
         */
        implicit val mongoConnection =
          if (!config.username.isEmpty && !config.password.isEmpty)
            Some(new MongoConnection(Some(config.username), Some(config.password)))
          else Some(new MongoConnection)

        /*
          Process Code Points for LA lookups
         */
        val resultForCodePoint = ProcessAddressBaseFiles.codePoints(config.codePoint)
        /*
          Log result summary
        */
        resultForCodePoint.outcome match {
          case Success => logger.info("Completed processing codepoint: \n" + resultForCodePoint.message)
          case Failure => {
            logger.info("Failed processing codepoint: \n" + resultForCodePoint.message)
            sys.exit()
          }
          case _ => {
            logger.info("Failed processing: Unable to generate a result]")
            sys.exit()
          }
        }

        /*
          Add indexes on codepoints
         */
        logger.info("adding codepoint indexes")
        mongoConnection.foreach(_.addCodePointIndexes())

        /*
          Process all streets into mongo first for reference
         */
        val resultForStreets = ProcessAddressBaseFiles.streets(config.dir)

        /*
          Log result summary
         */
        resultForStreets.outcome match {
          case Success => logger.info("Completed processing streets: \n" + resultForStreets.message)
          case Failure => {
            logger.info("Failed processing streets: \n" + resultForStreets.message)
            sys.exit()
          }
          case _ => {
            logger.info("Failed processing: Unable to generate a result]")
            sys.exit()
          }
        }

        /*
          Add indexes on streets
         */
        logger.info("adding street indexes")
        mongoConnection.foreach(_.addStreetIndexes())

        /*
          Process files a second time, now for address objects
          This requires the streets to be in mongo already
         */
        val resultForAddresses = ProcessAddressBaseFiles.addresses(config.dir)

        /*
          Add indexes to address rows
         */
        logger.info("adding indexes")
        mongoConnection.foreach(_.addIndexes())

        /*
          Log result summary
         */
        resultForAddresses.outcome match {
          case Success => logger.info("Completed processing: \n" + resultForAddresses.message)
          case Failure => {
            logger.info("Failed processing: \n" + resultForAddresses.message)
            sys.exit()
          }
          case _ => {
            logger.info("Failed processing: Unable to generate a result]")
            sys.exit()
          }
        }

        logger.info("Finshed Processing: " + config.dir + " in " + ((new DateTime).getMillis - start.getMillis) / 1000 / 60 + " minutes")

      }
    }
  }
}
