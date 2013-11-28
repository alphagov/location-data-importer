package uk.gov.gds.io

import org.specs2.mutable.Specification
import java.io.File
import org.specs2.specification.AfterExample
import uk.gov.gds.logging.Reporter
import uk.gov.ReporterTestUtils._
import org.specs2.mock.Mockito

class ProcessAddressBaseFilesTests extends Specification with AfterExample with Mockito {

  sequential

  def reportLineToTest(fileName: String) = reportLines.filter(_.startsWith(fileName)).headOption

  "The supplied file path" should {
    "be checked for existence" in {
      ProcessAddressBaseFiles.addresses("/tmp/shouldnotbehere")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.addresses("/tmp/shouldnotbehere")(None).message must beEqualTo("Supplied path does not exist")
      ProcessAddressBaseFiles.streets("/tmp/shouldnotbehere")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.streets("/tmp/shouldnotbehere")(None).message must beEqualTo("Supplied path does not exist")
    }

    "be checked as a directory" in {
      new File("/tmp/testfile.txt").createNewFile()
      ProcessAddressBaseFiles.addresses("/tmp/testfile.txt")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.addresses("/tmp/testfile.txt")(None).message must beEqualTo("Supplied path is not a directory")
      ProcessAddressBaseFiles.streets("/tmp/testfile.txt")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.streets("/tmp/testfile.txt")(None).message must beEqualTo("Supplied path is not a directory")
    }

    "be checked for having files to process" in {
      new File("/tmp/testdir").mkdir()
      ProcessAddressBaseFiles.addresses("/tmp/testdir")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.addresses("/tmp/testdir")(None).message must beEqualTo("/tmp/testdir contains no files")
      ProcessAddressBaseFiles.streets("/tmp/testdir")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.streets("/tmp/testdir")(None).message must beEqualTo("/tmp/testdir contains no files")
    }

    "only allow csv file when uploading streets" in {
      new File("/tmp/testdir").mkdir()
      new File("/tmp/testdir/noncsv.txt") createNewFile()
      new File("/tmp/testdir/ok.csv").createNewFile()
      ProcessAddressBaseFiles.addresses("/tmp/testdir")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.addresses("/tmp/testdir")(None).message must beEqualTo("/tmp/testdir contains files that are not csv files [/tmp/testdir/noncsv.txt]")
    }

    "only allow csv file when uploading addresses" in {
      new File("/tmp/testdir").mkdir()
      new File("/tmp/testdir/noncsv.txt") createNewFile()
      new File("/tmp/testdir/ok.csv").createNewFile()
      ProcessAddressBaseFiles.addresses("/tmp/testdir")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.addresses("/tmp/testdir")(None).message must beEqualTo("/tmp/testdir contains files that are not csv files [/tmp/testdir/noncsv.txt]")
    }

    "be ok if is a directory containing files when processing streets" in {
      new File("/tmp/testdir/").mkdir()
      new File("/tmp/testdir/tmp.csv").createNewFile()

      ProcessAddressBaseFiles.addresses("/tmp/testdir/")(None).outcome must beEqualTo(Success)
      ProcessAddressBaseFiles.addresses("/tmp/testdir/")(None).message must beEqualTo("processed addresses: [1] files")
    }

    "be ok if is a directory containing files when processing addreses" in {
      new File("/tmp/testdir/").mkdir()
      new File("/tmp/testdir/tmp.csv").createNewFile()

      ProcessAddressBaseFiles.addresses("/tmp/testdir/")(None).outcome must beEqualTo(Success)
      ProcessAddressBaseFiles.addresses("/tmp/testdir/")(None).message must beEqualTo("processed addresses: [1] files")
    }

    "correctly process a 'good' file returning count of processed files" in {
      ProcessAddressBaseFiles.addresses("testdata/single-good-file")(None).outcome must beEqualTo(Success)
      ProcessAddressBaseFiles.addresses("testdata/single-good-file")(None).message must beEqualTo("processed addresses: [1] files")
      ProcessAddressBaseFiles.streets("testdata/single-good-file")(None).outcome must beEqualTo(Success)
      ProcessAddressBaseFiles.streets("testdata/single-good-file")(None).message must beEqualTo("processed streets: [1] files")
    }

    "correctly process a number 'good' files returning count of all processed files" in {
      ProcessAddressBaseFiles.addresses("testdata/multiple-good-files")(None).outcome must beEqualTo(Success)
      ProcessAddressBaseFiles.addresses("testdata/multiple-good-files")(None).message must beEqualTo("processed addresses: [2] files")
      ProcessAddressBaseFiles.streets("testdata/multiple-good-files")(None).outcome must beEqualTo(Success)
      ProcessAddressBaseFiles.streets("testdata/multiple-good-files")(None).message must beEqualTo("processed streets: [2] files")
    }

    "correctly process a 'bad' file returning error message against file name when processing streets" in {
      ProcessAddressBaseFiles.streets("testdata/single-bad-file")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.streets("testdata/single-bad-file")(None).message must beEqualTo("processed streets: 0 files successfully and 1 files with errors")
      reportLineToTest("bad-file.csv") must not be None
      reportLineToTest("bad-file.csv").get must contain("row-parse-error")
      reportLineToTest("bad-file.csv").get must contain("15|BADSTREET-1")
    }

    "correctly process a 'bad' file returning error message against file name when processing addresses" in {
      ProcessAddressBaseFiles.addresses("testdata/single-bad-file")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.addresses("testdata/single-bad-file")(None).message must beEqualTo("processed addresses: 0 files successfully and 1 files with errors")
      reportLineToTest("bad-file.csv") must not be None
      reportLineToTest("bad-file.csv").get must contain("row-parse-error")
      reportLineToTest("bad-file.csv").get must contain("32|BADROW")
    }

    "correctly process a set of 'bad' files returning errors by file name when processing streets" in {
      ProcessAddressBaseFiles.streets("testdata/multiple-bad-files")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.streets("testdata/multiple-bad-files")(None).message must beEqualTo("processed streets: 0 files successfully and 2 files with errors")
      reportLineToTest("bad-file-1.csv") must not be None
      reportLineToTest("bad-file-1.csv").get must contain("row-parse-error")
      reportLineToTest("bad-file-1.csv").get must contain("15|BADSTREET-2")
      reportLineToTest("bad-file-2.csv") must not be None
      reportLineToTest("bad-file-2.csv").get must contain("row-parse-error")
      reportLineToTest("bad-file-2.csv").get must contain("15|BADSTREET")
    }

    "correctly process a set of 'bad' files returning errors by file name when processing addresses" in {
      ProcessAddressBaseFiles.addresses("testdata/multiple-bad-files")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.addresses("testdata/multiple-bad-files")(None).message must beEqualTo("processed addresses: 0 files successfully and 2 files with errors")
      reportLineToTest("bad-file-1.csv") must not be None
      reportLineToTest("bad-file-1.csv").get must contain("row-parse-error")
      reportLineToTest("bad-file-1.csv").get must contain("21|BADROW-1")
      reportLineToTest("bad-file-2.csv") must not be None
      reportLineToTest("bad-file-2.csv").get must contain("row-parse-error")
      reportLineToTest("bad-file-2.csv").get must contain("21|BADROW-2")
    }

    "correctly process a set of 'good' and 'bad' files returning errors by file name when processing streets" in {
      ProcessAddressBaseFiles.streets("testdata/good-and-bad-files")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.streets("testdata/good-and-bad-files")(None).message must beEqualTo("processed streets: 1 files successfully and 1 files with errors")
      reportLineToTest("bad-file-3.csv") must not be None
      reportLineToTest("bad-file-3.csv").get must contain("row-parse-error")
      reportLineToTest("bad-file-3.csv").get must contain("15|BADSTREET-3")
    }

    "correctly process a set of 'good' and 'bad' files returning errors by file name when processing addresses" in {
      ProcessAddressBaseFiles.addresses("testdata/good-and-bad-files")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.addresses("testdata/good-and-bad-files")(None).message must beEqualTo("processed addresses: 1 files successfully and 1 files with errors")
      reportLineToTest("bad-file-3.csv") must not be None
      reportLineToTest("bad-file-3.csv").get must contain("row-parse-error")
      reportLineToTest("bad-file-3.csv").get must contain("32|BADROW-1")
    }
  }

  def after {
    if (new File("/tmp/testdir").exists()) {
      directoryContents("/tmp/testdir").foreach(f => f.delete())
      new File("/tmp/testdir").delete()
    }

    if (new File(Reporter.reportFile).exists()) new File(Reporter.reportFile).delete()
  }

}
