package uk.gov.gds.io

import org.specs2.mutable._
import java.io.File

class FileUtilityMethodTests extends Specification  {

  "The file utilities" should {

    "be able to correctly check a file / directory doesn't exist" in {
      fileExists("testdata") must beEqualTo(true)
      fileExists("nonsense") must beEqualTo(false)
    }

    "be able to be identify a directory" in {
      isDirectory("testdata") must beEqualTo(true)
      isDirectory("testdata/single-good-file/good-file.csv") must beEqualTo(false)
    }

    "be able to create a list of all files in a directory" in {
      directoryContents("testdata").size must beEqualTo(6)
      directoryContents("testdata").map {
        _.getName
      } must contain("good-and-bad-files", "single-good-file", "multiple-good-files", "single-bad-file", "multiple-bad-files", "README.md").exactly
    }

    "be able to filter the list of files in a directory" in {
      filteredDirectoryContents("testdata/single-good-file", (file: File) => file.getName.endsWith(".csv")).size must beEqualTo(1)
      filteredDirectoryContents("testdata/single-good-file", (file: File) => file.getName.endsWith(".csv")).head.getName must beEqualTo("good-file.csv")
    }
  }

  "CSV Parsing" should {

    "be able to handle standard csv" in  {
      val line = parseCsvLine("1,2,3,4")
      line.size must beEqualTo(4)
      line(0) must beEqualTo("1")
      line(1) must beEqualTo("2")
      line(2) must beEqualTo("3")
      line(3) must beEqualTo("4")
    }

    "be able to handle quoted csv" in  {
      val line = parseCsvLine("\"1\",\"2\",\"3\",\"4\"")
      line.size must beEqualTo(4)
      line(0) must beEqualTo("1")
      line(1) must beEqualTo("2")
      line(2) must beEqualTo("3")
      line(3) must beEqualTo("4")
    }

    "be able to handle strings with spaces csv" in  {
      val line = parseCsvLine("1,2,string with some spaces,4")
      line.size must beEqualTo(4)
      line(0) must beEqualTo("1")
      line(1) must beEqualTo("2")
      line(2) must beEqualTo("string with some spaces")
      line(3) must beEqualTo("4")
    }

    "be able to handle embeded quotes" in  {
      val line = parseCsvLine("""1,2,string with a quote" and some spaces,4""")
      line.size must beEqualTo(4)
      line(0) must beEqualTo("1")
      line(1) must beEqualTo("2")
      line(2) must beEqualTo("""string with a quote" and some spaces""")
      line(3) must beEqualTo("4")
    }
  }
}
