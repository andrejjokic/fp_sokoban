package sokoban
package solver

import model.Matrix
import sokoban.common.{Directions, FieldTypes, Position}
import sokoban.common.FieldTypes.FieldType
import sokoban.common.Directions.Direction

import scala.annotation.tailrec
import scala.collection.mutable

object Solver {
  def solve(matrix: Matrix): Unit = {
    val visited = mutable.Set[MatrixSnapshot]()

    def _solve(matrix: Matrix): Boolean = {
      if (matrix.isGameCompleted) return true
      val snapshot = new MatrixSnapshot(matrix)
      if (visited(snapshot)) return false
      visited += snapshot

      Directions.sideDirections
        .filter(matrix.isMoveLegal)
        .foreach(direction => {
          if (_solve(move(matrix, direction))) return true
        })

      false
    }

    if (matrix.checkValidity != matrix.MAP_IS_CORRECT) {
      println("MAP IS INCORRECT.")
      return
    }

    println(_solve(matrix))
  }

  private def move(matrix: Matrix, direction: Direction): Matrix = {
    val ret = matrix.copy
    ret.movePlayer(direction)
    ret
  }
}
