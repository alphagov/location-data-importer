package uk.gov.gds.location.importer.processors

import org.specs2.mutable.{Before, Specification}
import uk.gov.gds.location.importer.mongo.MongoConnection
import org.specs2.mock.Mockito
import java.io.File

class AddressBaseFileProcessorTests extends Specification with Mockito {

  val mongoConnection = mock[MongoConnection]
  val addressBaseFileProcessor = new AddressBaseFileProcessor(mongoConnection)

  mongoConnection.insertCodePoints(any) returns (100)
  mongoConnection.insertStreets(any) returns (100)
  mongoConnection.insertAddresses(any) returns (100)

  "processCodePointFile" should {
    "successfully process a valid file" in {
      addressBaseFileProcessor.processCodePointFile(new File("testdata/codepoint/good-file.csv")) must beTrue
      there were atMostTwo(mongoConnection).insertCodePoints(any)
    }

    "successfully process a valid file with one bad row" in {
      addressBaseFileProcessor.processCodePointFile(new File("testdata/codepoint/file-with-one-bad-row.csv")) must beTrue
      there were atMostTwo(mongoConnection).insertCodePoints(any)
    }
  }

  "processAddressBaseForStreets" should {
    "successfully process a valid file" in {
      addressBaseFileProcessor.processAddressBaseForStreets(new File("testdata/addressbase/single-good-file/good-file.csv")) must beTrue
      there were one(mongoConnection).insertStreets(any)
    }

    "fail to process a file with a bad row" in {
      addressBaseFileProcessor.processAddressBaseForStreets(new File("testdata/addressbase/single-bad-file/bad-file.csv")) must beFalse
      there were noCallsTo(mongoConnection)
    }
  }

  "processAddressBaseForAddresses" should {
    "successfully process a valid file" in {
      addressBaseFileProcessor.processAddressBaseForAddresses(new File("testdata/addressbase/single-good-file/good-file.csv")) must beTrue
      there were one(mongoConnection).insertAddresses(any)
    }

    "fail to process a file with a bad row" in {
      addressBaseFileProcessor.processAddressBaseForAddresses(new File("testdata/addressbase/single-bad-file/bad-file.csv")) must beFalse
      there were noCallsTo(mongoConnection)
    }
  }

}
