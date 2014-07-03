package uk.gov.gds.location.importer

import com.mongodb.casbah.commons.conversions.scala.RegisterJodaTimeConversionHelpers
import scopt.OptionParser
import org.joda.time.DateTime
import uk.gov.gds.location.importer.io.FileUtilities._
import uk.gov.gds.location.importer.mongo.MongoConnection
import uk.gov.gds.location.importer.logging.Logging
import uk.gov.gds.location.importer.processors.AddressBaseFileProcessor

/**
 * Main class for data conversions from Ordinance Survey types into Locate style objects
 */
object LocationDataImporter extends Logging {

  case class Config(dir: String = "", codePoint: String = "", addressOnly: Boolean = false, username: String = "", password: String = "")

  def logStartOfRun() {
    logger.info("=== Starting Run at " + new DateTime + " ===")
  }

  def logEndOfRun() {
    logger.info("=== Ending Run at " + new DateTime + " ===")
  }

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
      opt[Unit]('o', "addressOnly") text "Only do address import stage. (Default process all stages)" action {
        (_, c: Config) => c.copy(addressOnly = true)
      }
      opt[String]('p', "password") text "Password for the mongo (default none)" action {
        (p: String, c: Config) => c.copy(password = p)
      }
      opt[String]('u', "username") text "Username for the mongo (default none)" action {
        (p: String, c: Config) => c.copy(username = p)
      }
      version("version") text "0.1"
    }

    opts.parse(args, Config()) map {
      config => {
        logStartOfRun()
        val start = new DateTime

        val mongoConnection = new MongoConnection

        /*
          Authenticate the mongo connection
         */
        if (!config.username.isEmpty && !config.password.isEmpty)
          mongoConnection.authenticate(config.username, config.password)

        /*
          Drops existing collections prior to new run
         */
        mongoConnection.dropAll()

        /*
          Create main processors
         */
        val addressBaseProcessor = new ProcessAddressBaseFiles(
          new AddressBaseFileProcessor(mongoConnection)
        )

        /*
          Only process streets and code points if required.
                   */
        if (!config.addressOnly) {
          processCodePoint(config, addressBaseProcessor, mongoConnection)
          processStreets(config, addressBaseProcessor, mongoConnection)
        }

        /*
          Process files for address objects
          This requires the streets and code point files to be in the database already
        */
        processAddresses(config, addressBaseProcessor, mongoConnection)

        logEndOfRun()

        logger.info("Finished Processing in " + ((new DateTime).getMillis - start.getMillis) / 1000 / 60 + " minutes")
      }
    }
  }


  private def processAddresses(config: Config, processors: ProcessAddressBaseFiles, mongoConnection: MongoConnection) = {

    val resultForAddresses = processors.processAddressBaseFilesForAddresses(config.dir)

    /*
      Add indexes to address rows
     */
    logger.info("adding indexes")
    mongoConnection.addAddressIndexes()

    /*
      Log result summary
     */
    logResults("addresses", resultForAddresses)
  }

  private def processStreets(config: Config, processors: ProcessAddressBaseFiles, mongoConnection: MongoConnection) = {
    /*
      Process all addressbase files for street data
    */
    val resultForStreets = processors.processAddressBaseFilesForStreets(config.dir)

    /*
      Add indexes on streets
     */
    logger.info("adding street indexes")
    mongoConnection.addStreetIndexes()

    /*
     Log result summary
    */
    logResults("streets", resultForStreets)

  }

  private def processCodePoint(config: Config, processors: ProcessAddressBaseFiles, mongoConnection: MongoConnection) = {
    /*
       Process Code Points for Local Authority code lookups
      */
    val resultForCodePoint = processors.processCodePointFiles(config.codePoint)

    /*
      Add indexes on codepoints
     */
    logger.info("adding codepoint indexes")
    mongoConnection.addCodePointIndexes()

    /*
      Log result summary
    */
    logResults("codepoint", resultForCodePoint)
  }

  /**
   * Log the results of the run
   * @param result
   */
  private def logResults(description: String, result: Result) {
    result.outcome match {
      case Success => logger.info(String.format("Completed processing %s %s", description, result.message))
      case Failure => {
        logger.error(String.format("Failed processing %s %s", description, result.message))
        sys.exit()
      }
      case _ => {
        logger.error(String.format("Failed processing %s: Unable to generate a result", description))
        sys.exit()
      }
    }
  }

}
