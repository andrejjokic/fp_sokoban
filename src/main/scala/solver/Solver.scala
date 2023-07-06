package sokoban
package solver

import model.Matrix
import sokoban.common.{Directions, FieldTypes, Position}
import sokoban.common.FieldTypes.FieldType
import sokoban.common.Directions.Direction
import sokoban.utils.{Converter, IOParser}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solver {

  private val DEFAULT_SOLUTION_PATH = "./solution.txt"

  def solve(matrix: Matrix): String = {
    val visited = mutable.Set[MatrixSnapshot]()
    val directions = ListBuffer[Direction]()

    def _solve(matrix: Matrix): Boolean = {
      if (matrix.isGameCompleted) return true
      val snapshot = new MatrixSnapshot(matrix)
      if (visited(snapshot)) return false
      visited += snapshot

      Directions.sideDirections
        .filter(matrix.isMoveLegal)
        .foreach(direction => {
          if (_solve(move(matrix, direction))) {
            directions += direction
            return true
          }
        })

      false
    }

    if (matrix.checkValidity != matrix.MAP_IS_CORRECT) {
      return "Map is not valid."
    }

    if (_solve(matrix)) {
      IOParser.write(directions.toList.reverse.map(Converter.toChar), DEFAULT_SOLUTION_PATH)
      "Solution was found and saved in " + DEFAULT_SOLUTION_PATH
    }
    else {
      "Solution could not be found. :("
    }
  }

  private def move(matrix: Matrix, direction: Direction): Matrix = {
    val ret = matrix.copy
    ret.movePlayer(direction)
    ret
  }
}
