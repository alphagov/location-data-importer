package uk.gov.gds.location.importer.io

import org.specs2.mutable.Specification
import java.io.File
import org.specs2.specification.AfterExample
import org.specs2.mock.Mockito

class ProcessAddressBaseFilesTests extends Specification {//} with AfterExample with Mockito {

//  sequential
//
//  "The supplied file path" should {
//    "be checked for existence" in {
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/shouldnotbehere")(None).outcome must beEqualTo(Failure)
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/shouldnotbehere")(None).message must beEqualTo("Supplied path does not exist")
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("/tmp/shouldnotbehere")(None).outcome must beEqualTo(Failure)
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("/tmp/shouldnotbehere")(None).message must beEqualTo("Supplied path does not exist")
//    }
//
//    "be checked as a directory" in {
//      new File("/tmp/testfile.txt").createNewFile()
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/testfile.txt")(None).outcome must beEqualTo(Failure)
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/testfile.txt")(None).message must beEqualTo("Supplied path is not a directory")
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("/tmp/testfile.txt")(None).outcome must beEqualTo(Failure)
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("/tmp/testfile.txt")(None).message must beEqualTo("Supplied path is not a directory")
//    }
//
//    "be checked for having files to process" in {
//      new File("/tmp/testdir").mkdir()
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/testdir")(None).outcome must beEqualTo(Failure)
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/testdir")(None).message must beEqualTo("/tmp/testdir contains no files")
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("/tmp/testdir")(None).outcome must beEqualTo(Failure)
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("/tmp/testdir")(None).message must beEqualTo("/tmp/testdir contains no files")
//    }
//
//    "only allow csv file when uploading streets" in {
//      new File("/tmp/testdir").mkdir()
//      new File("/tmp/testdir/noncsv.txt") createNewFile()
//      new File("/tmp/testdir/ok.csv").createNewFile()
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/testdir")(None).outcome must beEqualTo(Failure)
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/testdir")(None).message must beEqualTo("/tmp/testdir contains files that are not csv files [/tmp/testdir/noncsv.txt]")
//    }
//
//    "only allow csv file when uploading addresses" in {
//      new File("/tmp/testdir").mkdir()
//      new File("/tmp/testdir/noncsv.txt") createNewFile()
//      new File("/tmp/testdir/ok.csv").createNewFile()
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/testdir")(None).outcome must beEqualTo(Failure)
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/testdir")(None).message must beEqualTo("/tmp/testdir contains files that are not csv files [/tmp/testdir/noncsv.txt]")
//    }
//
//    "be ok if is a directory containing files when processing streets" in {
//      new File("/tmp/testdir/").mkdir()
//      new File("/tmp/testdir/tmp.csv").createNewFile()
//
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/testdir/")(None).outcome must beEqualTo(Success)
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/testdir/")(None).message must beEqualTo("processed addresses: [1] files")
//    }
//
//    "be ok if is a directory containing files when processing addreses" in {
//      new File("/tmp/testdir/").mkdir()
//      new File("/tmp/testdir/tmp.csv").createNewFile()
//
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/testdir/")(None).outcome must beEqualTo(Success)
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("/tmp/testdir/")(None).message must beEqualTo("processed addresses: [1] files")
//    }
//
//    "correctly process a 'good' file returning count of processed files" in {
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/addressbase/single-good-file")(None).outcome must beEqualTo(Success)
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/addressbase/single-good-file")(None).message must beEqualTo("processed addresses: [1] files")
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("testdata/addressbase/single-good-file")(None).outcome must beEqualTo(Success)
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("testdata/addressbase/single-good-file")(None).message must beEqualTo("processed streets: [1] files")
//    }
//
//    "correctly process a number 'good' files returning count of all processed files" in {
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/addressbase/multiple-good-files")(None).outcome must beEqualTo(Success)
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/addressbase/multiple-good-files")(None).message must beEqualTo("processed addresses: [2] files")
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("testdata/addressbase/multiple-good-files")(None).outcome must beEqualTo(Success)
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("testdata/addressbase/multiple-good-files")(None).message must beEqualTo("processed streets: [2] files")
//    }
//
//    "correctly process a 'bad' file returning error message against file name when processing streets" in {
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("testdata/addressbase/single-bad-file")(None).outcome must beEqualTo(Failure)
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("testdata/addressbase/single-bad-file")(None).message must beEqualTo("processed streets: 0 files successfully and 1 files with errors")
//      reportLineToTest("bad-file.csv") must not be None
//      reportLineToTest("bad-file.csv").get must contain("row-parse-error")
//      reportLineToTest("bad-file.csv").get must contain("15|BADSTREET-1")
//      processedLineToTest("bad-file.csv") must not be None
//      processedLineToTest("bad-file.csv").get must contain("streets")
//    }
//
//    "correctly process a 'bad' file returning error message against file name when processing addresses" in {
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/addressbase/single-bad-file")(None).outcome must beEqualTo(Failure)
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/addressbase/single-bad-file")(None).message must beEqualTo("processed addresses: 0 files successfully and 1 files with errors")
//      reportLineToTest("bad-file.csv") must not be None
//      reportLineToTest("bad-file.csv").get must contain("row-parse-error")
//      reportLineToTest("bad-file.csv").get must contain("32|BADROW")
//      processedLineToTest("bad-file.csv") must not be None
//      processedLineToTest("bad-file.csv").get must contain("addresses")
//    }
//
//    "correctly process a set of 'bad' files returning errors by file name when processing streets" in {
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("testdata/addressbase/multiple-bad-files")(None).outcome must beEqualTo(Failure)
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("testdata/addressbase/multiple-bad-files")(None).message must beEqualTo("processed streets: 0 files successfully and 2 files with errors")
//      reportLineToTest("bad-file-1.csv") must not be None
//      reportLineToTest("bad-file-1.csv").get must contain("row-parse-error")
//      reportLineToTest("bad-file-1.csv").get must contain("15|BADSTREET-2")
//      reportLineToTest("bad-file-2.csv") must not be None
//      reportLineToTest("bad-file-2.csv").get must contain("row-parse-error")
//      reportLineToTest("bad-file-2.csv").get must contain("15|BADSTREET")
//      processedLineToTest("bad-file-1.csv") must not be None
//      processedLineToTest("bad-file-1.csv").get must contain("streets")
//      processedLineToTest("bad-file-2.csv") must not be None
//      processedLineToTest("bad-file-2.csv").get must contain("streets")
//    }
//
//    "correctly process a set of 'bad' files returning errors by file name when processing addresses" in {
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/addressbase/multiple-bad-files")(None).outcome must beEqualTo(Failure)
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/addressbase/multiple-bad-files")(None).message must beEqualTo("processed addresses: 0 files successfully and 2 files with errors")
//      reportLineToTest("bad-file-1.csv") must not be None
//      reportLineToTest("bad-file-1.csv").get must contain("row-parse-error")
//      reportLineToTest("bad-file-1.csv").get must contain("21|BADROW-1")
//      reportLineToTest("bad-file-2.csv") must not be None
//      reportLineToTest("bad-file-2.csv").get must contain("row-parse-error")
//      reportLineToTest("bad-file-2.csv").get must contain("21|BADROW-2")
//      processedLineToTest("bad-file-1.csv") must not be None
//      processedLineToTest("bad-file-1.csv").get must contain("addresses")
//      processedLineToTest("bad-file-2.csv") must not be None
//      processedLineToTest("bad-file-2.csv").get must contain("addresses")
//    }
//
//    "correctly process a set of 'good' and 'bad' files returning errors by file name when processing streets" in {
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("testdata/addressbase/good-and-bad-files")(None).outcome must beEqualTo(Failure)
//      ProcessAddressBaseFiles.processAddressBaseFilesForStreets("testdata/addressbase/good-and-bad-files")(None).message must beEqualTo("processed streets: 1 files successfully and 1 files with errors")
//      reportLineToTest("bad-file-3.csv") must not be None
//      reportLineToTest("bad-file-3.csv").get must contain("row-parse-error")
//      reportLineToTest("bad-file-3.csv").get must contain("15|BADSTREET-3")
//      processedLineToTest("bad-file-3.csv") must not be None
//      processedLineToTest("bad-file-3.csv").get must contain("streets")
//    }
//
//    "correctly process a set of 'good' and 'bad' files returning errors by file name when processing addresses" in {
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/addressbase/good-and-bad-files")(None).outcome must beEqualTo(Failure)
//      ProcessAddressBaseFiles.processAddressBaseFilesForAddresses("testdata/addressbase/good-and-bad-files")(None).message must beEqualTo("processed addresses: 1 files successfully and 1 files with errors")
//      reportLineToTest("bad-file-3.csv") must not be None
//      reportLineToTest("bad-file-3.csv").get must contain("row-parse-error")
//      reportLineToTest("bad-file-3.csv").get must contain("32|BADROW-1")
//      processedLineToTest("bad-file-3.csv") must not be None
//      processedLineToTest("bad-file-3.csv").get must contain("addresses")
//    }
//  }
//
//  def after {
//    if (new File("/tmp/testdir").exists()) {
//      directoryContents("/tmp/testdir").foreach(f => f.delete())
//      new File("/tmp/testdir").delete()
//    }
//
//    if (new File(Reporter.reportFile).exists()) new File(Reporter.reportFile).delete()
//    if (new File(Reporter.processed).exists()) new File(Reporter.processed).delete()
//  }

}
