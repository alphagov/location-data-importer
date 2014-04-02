package uk.gov.gds.location.importer

import org.specs2.mutable.Specification
import org.specs2.mock.Mockito
import uk.gov.gds.location.importer.processors.AddressBaseFileProcessor
import uk.gov.gds.location.importer.io.FileUtilities._

class ProcessAddressBaseFilesTests extends Specification with Mockito {


  sequential

  val addressBaseFileProcessors = mock[AddressBaseFileProcessor]
  val processAddressBaseFiles = new ProcessAddressBaseFiles(addressBaseFileProcessors)

  "The supplied file path" should {
    "be checked for existence" in {
      processAddressBaseFiles.processCodePointFiles("/tmp/shouldnotbehere/").outcome must beEqualTo(Failure)
      processAddressBaseFiles.processCodePointFiles("/tmp/shouldnotbehere/").message must beEqualTo("Supplied path does not exist")
      processAddressBaseFiles.processAddressBaseFilesForStreets("/tmp/shouldnotbehere/").outcome must beEqualTo(Failure)
      processAddressBaseFiles.processAddressBaseFilesForStreets("/tmp/shouldnotbehere/").message must beEqualTo("Supplied path does not exist")
      processAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/shouldnotbehere/").outcome must beEqualTo(Failure)
      processAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/shouldnotbehere/").message must beEqualTo("Supplied path does not exist")
    }

    "be checked to be a directory" in {
      processAddressBaseFiles.processCodePointFiles("testdata/testfile.txt").outcome must beEqualTo(Failure)
      processAddressBaseFiles.processCodePointFiles("testdata/testfile.txt").message must beEqualTo("Supplied path is not a directory")
      processAddressBaseFiles.processAddressBaseFilesForStreets("testdata/testfile.txt").outcome must beEqualTo(Failure)
      processAddressBaseFiles.processAddressBaseFilesForStreets("testdata/testfile.txt").message must beEqualTo("Supplied path is not a directory")
      processAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/testfile.txt").outcome must beEqualTo(Failure)
      processAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/testfile.txt").message must beEqualTo("Supplied path is not a directory")
    }

    "be checked to be a directory with files" in {
      processAddressBaseFiles.processCodePointFiles("testdata/emptydirectoryusedintesting").outcome must beEqualTo(Failure)
      processAddressBaseFiles.processCodePointFiles("testdata/emptydirectoryusedintesting").message must beEqualTo("testdata/emptydirectoryusedintesting contains no files")
      processAddressBaseFiles.processAddressBaseFilesForStreets("testdata/emptydirectoryusedintesting").outcome must beEqualTo(Failure)
      processAddressBaseFiles.processAddressBaseFilesForStreets("testdata/emptydirectoryusedintesting").message must beEqualTo("testdata/emptydirectoryusedintesting contains no files")
      processAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/emptydirectoryusedintesting").outcome must beEqualTo(Failure)
      processAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/emptydirectoryusedintesting").message must beEqualTo("testdata/emptydirectoryusedintesting contains no files")
    }

    "be checked to be a directory with no non csv files" in {
      processAddressBaseFiles.processCodePointFiles("testdata/addressbase/single-bad-file").outcome must beEqualTo(Failure)
      processAddressBaseFiles.processCodePointFiles("testdata/addressbase/single-bad-file").message must beEqualTo("testdata/addressbase/single-bad-file contains files that are not csv files [testdata/addressbase/single-bad-file/bad-file.txt]")
      processAddressBaseFiles.processAddressBaseFilesForStreets("testdata/addressbase/single-bad-file").outcome must beEqualTo(Failure)
      processAddressBaseFiles.processAddressBaseFilesForStreets("testdata/addressbase/single-bad-file").message must beEqualTo("testdata/addressbase/single-bad-file contains files that are not csv files [testdata/addressbase/single-bad-file/bad-file.txt]")
      processAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/addressbase/single-bad-file").outcome must beEqualTo(Failure)
      processAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/addressbase/single-bad-file").message must beEqualTo("testdata/addressbase/single-bad-file contains files that are not csv files [testdata/addressbase/single-bad-file/bad-file.txt]")
    }

    "return a successful result when processed a code point directory" in {
      addressBaseFileProcessors.processCodePointFile(any) returns true
      val result = processAddressBaseFiles.processCodePointFiles("testdata/codepoint/good-files")
      result.outcome must beEqualTo(Success)
      result.message must beEqualTo("processed codepoint: [1] files")
    }

    "return a successful result when processed an address base directory for streets" in {
      addressBaseFileProcessors.processAddressBaseForStreets(any) returns true
      val result = processAddressBaseFiles.processAddressBaseFilesForStreets("testdata/addressbase/good-file")
      result.outcome must beEqualTo(Success)
      result.message must beEqualTo("processed streets: [1] files")
    }

    "return a successful result when processed an address base directory for addresses" in {
      addressBaseFileProcessors.processAddressBaseForAddresses(any) returns true
      val result = processAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/addressbase/good-file")
      result.outcome must beEqualTo(Success)
      result.message must beEqualTo("processed addresses: [1] files")
    }
  }

  "Results of processing good and bad street files" should {
    "return a successful result when processed an address base directory for streets - with count of good and bad files" in {
      addressBaseFileProcessors.processAddressBaseForStreets(any) returns true thenReturns false
      val result = processAddressBaseFiles.processAddressBaseFilesForStreets("testdata/addressbase/good-and-bad-files")
      result.outcome must beEqualTo(Failure)
      result.message must beEqualTo("processed streets: 1 files successfully and 1 files with errors")
    }
  }

  "Results of processing good and bad address files" should {
    "return a successful result when processed an address base directory for addresses - with count of good and bad files" in {
      addressBaseFileProcessors.processAddressBaseForAddresses(any) returns true thenReturns false
      val result = processAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/addressbase/good-and-bad-files")
      result.outcome must beEqualTo(Failure)
      result.message must beEqualTo("processed addresses: 1 files successfully and 1 files with errors")
    }

  }
}
