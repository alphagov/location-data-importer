package uk.gov.gds.io

import scalax.io._

object ProcessAddressBaseFiles {

  implicit val code: Codec = Codec("UTF-8")

  def loadFile(fileName: String) {
    val input: Input = Resource.fromFile(fileName)
    println(input.lines().mkString(":"))
  }

}
