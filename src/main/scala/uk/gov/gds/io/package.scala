package uk.gov.gds

import java.io.File
import com.Ostermiller.util.CSVParser

package object io {

  def fileExists(location: String) = new File(location).exists

  def isDirectory(location: String) = new File(location).isDirectory

  def directoryContents(location: String) = new File(location).listFiles().toList

  def filteredDirectoryContents(location: String, fileFilter: File => Boolean) = directoryContents(location).filter(fileFilter)

  def parseCsvLine(line: String) = CSVParser.parse(line)(0) // get first line from array of arrays
}
