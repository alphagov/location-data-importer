package uk.gov.gds.io

import org.specs2.mutable.Specification
import java.io.File
import org.specs2.specification.AfterExample

class ProcessAddressBaseFilesTests extends Specification with AfterExample {

  sequential

  "The supplied file path" should {
    "be checked for existence" in {
      ProcessAddressBaseFiles.process("/tmp/shouldnotbehere").outcome must beEqualTo(Some(Failure))
      ProcessAddressBaseFiles.process("/tmp/shouldnotbehere").messages(0) must beEqualTo("Supplied path does not exist")
    }
  }

  "be checked as a directory" in {
    new File("/tmp/testfile.txt").createNewFile()
    ProcessAddressBaseFiles.process("/tmp/testfile.txt").outcome must beEqualTo(Some(Failure))
    ProcessAddressBaseFiles.process("/tmp/testfile.txt").messages(0) must beEqualTo("Supplied path is not a directory")
  }

  "be checked for having files to process" in {
    new File("/tmp/testdir").mkdir()
    ProcessAddressBaseFiles.process("/tmp/testdir").outcome must beEqualTo(Some(Failure))
    ProcessAddressBaseFiles.process("/tmp/testdir").messages(0) must beEqualTo("/tmp/testdir contains no files")
  }

  "only allow csv files" in {
    new File("/tmp/testdir").mkdir()
    new File("/tmp/testdir/noncsv.txt") createNewFile()
    new File("/tmp/testdir/ok.csv").createNewFile()
    ProcessAddressBaseFiles.process("/tmp/testdir").outcome must beEqualTo(Some(Failure))
    ProcessAddressBaseFiles.process("/tmp/testdir").messages(0) must beEqualTo("/tmp/testdir contains files that are not csv files [/tmp/testdir/noncsv.txt]")
  }

  "be ok if is a directory containg files" in {
    new File("/tmp/testdir/").mkdir()
    new File("/tmp/testdir/tmp.csv").createNewFile()

    ProcessAddressBaseFiles.process("/tmp/testdir/").outcome must beEqualTo(Some(Success))
    ProcessAddressBaseFiles.process("/tmp/testdir/").messages(0) must beEqualTo("Processed [0] rows")
  }

  "correctly process a 'good' file returning count of processed rows" in {
    ProcessAddressBaseFiles.process("testdata/single-good-file").outcome must beEqualTo(Some(Success))
    ProcessAddressBaseFiles.process("testdata/single-good-file").messages(0) must beEqualTo("Processed [4] rows")
  }

  "correctly process a number 'good' files returning count of processed rows in all files" in {
    ProcessAddressBaseFiles.process("testdata/multiple-good-files").outcome must beEqualTo(Some(Success))
    ProcessAddressBaseFiles.process("testdata/multiple-good-files").messages(0) must beEqualTo("Processed [8] rows")
  }

  "correctly process a 'bad' file returning error message against file name" in {
    ProcessAddressBaseFiles.process("testdata/single-bad-file").outcome must beEqualTo(Some(Failure))
    ProcessAddressBaseFiles.process("testdata/single-bad-file").messages(0) must beEqualTo("ROW Error filename [bad-file.csv] row [11, BADROW]")
  }

  "correctly process a set of 'bad' files returning errors by file name" in {
    ProcessAddressBaseFiles.process("testdata/multiple-bad-files").outcome must beEqualTo(Some(Failure))
    ProcessAddressBaseFiles.process("testdata/multiple-bad-files").messages must contain("ROW Error filename [bad-file-1.csv] row [11, BADROW-1]")
    ProcessAddressBaseFiles.process("testdata/multiple-bad-files").messages must contain("ROW Error filename [bad-file-2.csv] row [11, BADROW-2]")
  }

  def after {
    if (new File("/tmp/testdir").exists()) {
      directoryContents("/tmp/testdir").foreach(f => f.delete())
      new File("/tmp/testdir").delete()
    }
  }
}
