package uk.gov.gds.io

import org.specs2.mutable.Specification
import java.io.File
import uk.gov.gds.model.{Failure, Success, Result}

class ProcessAddressBaseFilesTests extends Specification  {

  sequential

  "The supplied file path" should {
    "be checked for existence" in {
      ProcessAddressBaseFiles.process("/tmp/shouldnotbehere").outcome must beEqualTo(Failure)
      ProcessAddressBaseFiles.process("/tmp/shouldnotbehere").message must beEqualTo("Supplied path does not exist")
    }
  }

  "be checked as a directory" in {
    new File("/tmp/testfile.txt").createNewFile()
    ProcessAddressBaseFiles.process("/tmp/testfile.txt").outcome must beEqualTo(Failure)
    ProcessAddressBaseFiles.process("/tmp/testfile.txt").message must beEqualTo("Supplied path is not a directory")
    new File("/tmp/testfile.txt").delete()
  }

  "be checked for having files to process" in {
    new File("/tmp/testdir").mkdir()
    ProcessAddressBaseFiles.process("/tmp/testdir").outcome must beEqualTo(Failure)
    ProcessAddressBaseFiles.process("/tmp/testdir").message must beEqualTo("/tmp/testdir contains no files")
    new File("/tmp/testdir").delete()
  }

  "be ok if is a directory containg files" in {
    new File("/tmp/testdir/").mkdir()
    new File("/tmp/testdir/tmp.txt").createNewFile()

    ProcessAddressBaseFiles.process("/tmp/testdir/").outcome must beEqualTo(Success)
    ProcessAddressBaseFiles.process("/tmp/testdir/").message must beEqualTo("Processed files")

    new File("/tmp/testdir/tmp.txt").delete()
    new File("/tmp/testdir").delete()
  }
}
