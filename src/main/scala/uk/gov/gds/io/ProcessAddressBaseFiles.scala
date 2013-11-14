package uk.gov.gds.io

import scalax.io._

object ProcessAddressBaseFiles {

  def process(filePath: String): Result = {
    filePathHasErrors(filePath) match {
      case Some(error) => error
      case _ => processFiles(filePath)
    }
  }

  private def processFiles(filePath: String) = {
    Result(Success, "Processed files")
  }

  private def filePathHasErrors(filePath: String) = {
    if (!fileExists(filePath)) Some(Result(Failure, "Supplied path does not exist"))
    else if (!isDirectory(filePath)) Some(Result(Failure, "Supplied path is not a directory"))
    else if (directoryContents(filePath).isEmpty) Some(Result(Failure, filePath + " contains no files"))
    else None
  }

  def loadFile(fileName: String) {
    val input: Input = Resource.fromFile(fileName)
    println(input.lines().mkString(":"))
  }

}
