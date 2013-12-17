package uk.gov.gds

import scopt.OptionParser
import uk.gov.gds.io.ProcessAddressBaseFiles
import uk.gov.gds.logging.{Reporter, Logging}
import uk.gov.gds.io.{Failure, Success}
import java.io.File
import com.mongodb.casbah.commons.conversions.scala.RegisterJodaTimeConversionHelpers
import org.joda.time.DateTime

object LocationDataImporter extends Logging {

  case class Config(dir: String = "", codePoint: String = "", persist: Boolean = false, cleanReport: Boolean = false, index: Boolean = false, username: String = "", password: String = "")

  def main(args: Array[String]) {

    RegisterJodaTimeConversionHelpers()

    val opts = new OptionParser[Config]("Location Data Importer") {
      head("Parse and import location data", "0.1")
      opt[String]('d', "dir") required() text "Location of address base files files" action {
        (dir: String, c: Config) => c.copy(dir = dir)
      }
      opt[String]('f', "code point") required() text "Location of code point files)" action {
        (file: String, c: Config) => c.copy(codePoint = file)
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
      opt[Unit]('c', "cleanReport") text "Clean the report. Default false" action {
        (_, c: Config) => c.copy(cleanReport = true)
      }
      opt[String]('p', "password") text "Password for the mongo" action {
        (p: String, c: Config) => c.copy(password = p)
      }
      help("help") text "use -d or -dir to identify source directory containing files to parse"
      version("version") text "0.1"
    }

    opts.parse(args, Config()) map {
      config => {
        val start = new DateTime
        logger.info("Started processing: " + config.dir + " Persisting: " + config.persist)

        if(config.cleanReport) {
          new File(Reporter.reportFile).delete()
          new File(Reporter.processed).delete()
        }

        /*
          Initialize the mongo connection
         */
        implicit val mongoConnection = config.persist match {
          case true if !config.username.isEmpty && !config.password.isEmpty => Some(new MongoConnection(Some(config.username), Some(config.password)))
          case true => Some(new MongoConnection)
          case false => None
        }


        /*
          Process Code Points for LA lookups
         */
        val resultForCodePoint = ProcessAddressBaseFiles.codePoints(config.codePoint)
        /*
          Log result summary
        */
        resultForCodePoint.outcome match {
          case Success => logger.info("Completed processing codepoint: \n" + resultForCodePoint.message)
          case Failure => logger.info("Failed processing codepoint: \n" + resultForCodePoint.message)
          case _ => logger.info("Failed processing: Unable to generate a result]")
        }

        /*
          Add indexes on codepoints
         */
        if (config.index) {
          logger.info("adding codepoint indexes")
          mongoConnection.foreach(_.addCodePointIndexes())
        }

        /*
          Process all streets into mongo first for reference
         */
        val resultForStreets = ProcessAddressBaseFiles.streets(config.dir)

        /*
          Log result summary
         */
        resultForStreets.outcome match {
          case Success => logger.info("Completed processing streets: \n" + resultForStreets.message)
          case Failure => logger.info("Failed processing streets: \n" + resultForStreets.message)
          case _ => logger.info("Failed processing: Unable to generate a result]")
        }

        /*
          Add indexes on streets
         */
        if (config.index) {
          logger.info("adding street indexes")
          mongoConnection.foreach(_.addStreetIndexes())
        }

        /*
          Process files a second time, now for address objects
          This requires the streets to be in mongo already
         */
        val resultForAddresses = ProcessAddressBaseFiles.addresses(config.dir)

        /*
          Add indexes to address rows
         */
        if (config.index) {
          logger.info("adding indexes")
          mongoConnection.foreach(_.addIndexes())
        }

        /*
          Log result summary
         */
        resultForAddresses.outcome match {
          case Success => logger.info("Completed processing: \n" + resultForAddresses.message)
          case Failure => logger.info("Failed processing: \n" + resultForAddresses.message)
          case _ => logger.info("Failed processing: Unable to generate a result]")
        }

        logger.info("Finshed Processing: " + config.dir + " in " + ((new DateTime).getMillis - start.getMillis) / 1000 / 60 + " minutes")

      }
    }
  }
}
