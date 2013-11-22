package uk.gov.gds.io

import org.specs2.mutable.Specification
import java.io.File
import org.specs2.specification.AfterExample

class ProcessAddressBaseFilesTests extends Specification with AfterExample {

  sequential

  "The supplied file path" should {
    "be checked for existence" in {
      ProcessAddressBaseFiles.process("/tmp/shouldnotbehere")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.process("/tmp/shouldnotbehere")(None).messages(0) must beEqualTo("Supplied path does not exist")
    }

    "be checked as a directory" in {
      new File("/tmp/testfile.txt").createNewFile()
      ProcessAddressBaseFiles.process("/tmp/testfile.txt")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.process("/tmp/testfile.txt")(None).messages(0) must beEqualTo("Supplied path is not a directory")
    }

    "be checked for having files to process" in {
      new File("/tmp/testdir").mkdir()
      ProcessAddressBaseFiles.process("/tmp/testdir")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.process("/tmp/testdir")(None).messages(0) must beEqualTo("/tmp/testdir contains no files")
    }

    "only allow csv files" in {
      new File("/tmp/testdir").mkdir()
      new File("/tmp/testdir/noncsv.txt") createNewFile()
      new File("/tmp/testdir/ok.csv").createNewFile()
      ProcessAddressBaseFiles.process("/tmp/testdir")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.process("/tmp/testdir")(None).messages(0) must beEqualTo("/tmp/testdir contains files that are not csv files [/tmp/testdir/noncsv.txt]")
    }

    "be ok if is a directory containing files" in {
      new File("/tmp/testdir/").mkdir()
      new File("/tmp/testdir/tmp.csv").createNewFile()

      ProcessAddressBaseFiles.process("/tmp/testdir/")(None).outcome must beEqualTo(Success)
      ProcessAddressBaseFiles.process("/tmp/testdir/")(None).messages(0) must beEqualTo("processed=[1] files")
    }

    "correctly process a 'good' file returning count of processed fiels" in {
      ProcessAddressBaseFiles.process("testdata/single-good-file")(None).outcome must beEqualTo(Success)
      ProcessAddressBaseFiles.process("testdata/single-good-file")(None).messages(0) must beEqualTo("processed=[1] files")
    }

    "correctly process a number 'good' files returning count of all processed files" in {
      ProcessAddressBaseFiles.process("testdata/multiple-good-files")(None).outcome must beEqualTo(Success)
      ProcessAddressBaseFiles.process("testdata/multiple-good-files")(None).messages(0) must beEqualTo("processed=[2] files")
    }

    "correctly process a 'bad' file returning error message against file name" in {
      ProcessAddressBaseFiles.process("testdata/single-bad-file")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.process("testdata/single-bad-file")(None).messages(0) must beEqualTo("bad-file.csv")
    }

    "correctly process a set of 'bad' files returning errors by file name" in {
      ProcessAddressBaseFiles.process("testdata/multiple-bad-files")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.process("testdata/multiple-bad-files")(None).messages must contain("bad-file-1.csv")
      ProcessAddressBaseFiles.process("testdata/multiple-bad-files")(None).messages must contain("bad-file-2.csv")
    }

    "correctly process a set of 'good' and 'bad' files returning errors by file name" in {
      ProcessAddressBaseFiles.process("testdata/good-and-bad-files")(None).outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.process("testdata/good-and-bad-files")(None).messages(0) must beEqualTo("processed=[1] files")
      ProcessAddressBaseFiles.process("testdata/good-and-bad-files")(None).messages must contain("bad-file-1.csv")
    }
  }

  def after {
    if (new File("/tmp/testdir").exists()) {
      directoryContents("/tmp/testdir").foreach(f => f.delete())
      new File("/tmp/testdir").delete()
    }
  }
}
