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

//  def solve_dfs(matrix: Matrix): String = {
//    val visited = mutable.Set[MatrixSnapshot]()
//    val directions = ListBuffer[Direction]()
//
//    def _solve(matrix: Matrix): Boolean = {
//      if (matrix.isGameCompleted) return true
//      val snapshot = new MatrixSnapshot(matrix)
//      if (visited(snapshot)) return false
//      visited += snapshot
//
//      Directions.sideDirections
//        .filter(matrix.isMoveLegal)
//        .foreach(direction => {
//          if (_solve(move(matrix, direction))) {
//            directions += direction
//            return true
//          }
//        })
//
//      false
//    }
//
//    if (matrix.checkValidity != matrix.MAP_IS_CORRECT) {
//      return "Map is not valid."
//    }
//
//    if (_solve(matrix)) {
//      IOParser.write(directions.toList.reverse.map(Converter.toChar), DEFAULT_SOLUTION_PATH)
//      "Solution was found and saved in " + DEFAULT_SOLUTION_PATH
//    }
//    else {
//      "Solution could not be found. :("
//    }
//  }

  def solve(matrix: Matrix): String = {
    val visited = mutable.Set[MatrixSnapshot]()
    val queue = mutable.PriorityQueue[MatrixSnapshot]()

    var cnt = 0

    @tailrec
    def _solve(): Option[MatrixSnapshot] = {
      if (queue.isEmpty) return None
      val snapshot = queue.dequeue()
      val matrix = snapshot.matrix

      for (direction <- Directions.sideDirections.filter(matrix.isMoveLegal)) {
        val neighbour = move(matrix, direction)
        val neighbourSnapshot = new MatrixSnapshot(neighbour, snapshot, direction)

        if (neighbour.isGameCompleted) return Some(neighbourSnapshot)

        if (!visited(neighbourSnapshot)) {
          visited += neighbourSnapshot
          queue.enqueue(neighbourSnapshot)
        }

        cnt += 1
      }

      _solve()
    }

    if (matrix.checkValidity != matrix.MAP_IS_CORRECT) {
      return "Map is not valid."
    }

    val startPos = new MatrixSnapshot(matrix, None, None)
    visited += startPos
    queue.enqueue(startPos)

    val finish = _solve()
    println(cnt)

    if (finish.isDefined) {
      IOParser.write(reconstructPath(finish).map(Converter.toChar), DEFAULT_SOLUTION_PATH)
      "Solution was found and saved in " + DEFAULT_SOLUTION_PATH
    }
    else {
      "Solution could not be found. :("
    }
  }

  private def reconstructPath(finish: Option[MatrixSnapshot]): List[Direction] = {
    @tailrec
    def _reconstructPath(snapshot: Option[MatrixSnapshot], acc: List[Direction]): List[Direction] = snapshot.get.parent match
      case None => acc
      case Some(_) => _reconstructPath(snapshot.get.parent, snapshot.get.parentDirection.get :: acc)

    _reconstructPath(finish, Nil)
  }

  private def move(matrix: Matrix, direction: Direction): Matrix = {
    val ret = matrix.copy
    ret.movePlayer(direction)
    ret
  }
}
