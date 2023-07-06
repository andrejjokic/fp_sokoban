package sokoban
package utils

import sokoban.common.Directions.Direction

import java.io.{BufferedWriter, FileWriter, File}
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

  def write(moves: List[Char], filePath: String): Unit = {
    val bufferedWriter = new BufferedWriter(new FileWriter(new File(filePath)))
    try {
      bufferedWriter.write(moves.mkString("\n"))
    } finally {
      bufferedWriter.close()
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
