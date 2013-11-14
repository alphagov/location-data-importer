package uk.gov.gds.io

import scalax.io._
import uk.gov.gds.model.{Failure, Success, Result}

object ProcessAddressBaseFiles {

  def process(filePath: String): Result = {
    checkFilePath(filePath) match {
      case Some(result) => result
      case _ => processFiles(filePath)
    }
  }

  private def processFiles(filePath: String) = {
    Result(Success, "Processed files")
  }

  private def checkFilePath(filePath: String) = {
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
