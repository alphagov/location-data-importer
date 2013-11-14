package uk.gov.gds.io

import scalax.io._

object FileLoader {

  def loadFile(fileName: String) {
    val input: Input = Resource.fromFile("/tmp/tmpfile.txt");
    println(input.lines().mkString(":"))
  }

}
