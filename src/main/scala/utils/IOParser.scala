package sokoban
package utils

import scala.io.Source

object IOParser {
  def read(filePath: String): List[List[Char]] = {
    val bufferedSource = Source.fromFile(filePath)
    try {
      val lines = for (line <- bufferedSource.getLines) yield line.toUpperCase.toList
      lines.toList
    } finally {
      bufferedSource.close()
    }
  }

  def readCharPerLine(filePath: String): List[Char] = {
    val bufferedSource = Source.fromFile(filePath)
    try {
      val chars = for (line <- bufferedSource.getLines) yield line.toUpperCase.head
      chars.toList
    } finally {
      bufferedSource.close()
    }
  }
}
