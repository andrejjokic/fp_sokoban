package sokoban
package solver

import model.Matrix

import sokoban.common.{Directions, FieldTypes, Position}
import sokoban.common.FieldTypes.FieldType
import sokoban.common.Directions.Direction

import scala.collection.mutable

object Solver {
  def solve(matrix: Matrix): Unit = {
    def _solve(matrix: Matrix, parentDirection: Direction): Boolean = {
      if (matrix.isGameCompleted) return true

      for (direction <- Directions.sideDirections.filterNot(_.==(parentDirection))) {
        if (matrix.isMoveLegal(direction)) {
          val nextPos = matrix.playerPosition + matrix.directionOffset(direction)
          val me =
            if (isBox(matrix, nextPos)) direction
            else Directions.UP_LEFT // dummy

          if (_solve(move(matrix, direction), me)) return true
        }
      }

      false
    }

    if (matrix.checkValidity != matrix.MAP_IS_CORRECT) {
      println("MAP IS INCORRECT.")
      return
    }

    if (_solve(matrix, Directions.UP_LEFT)) println("RESENO")
    else println("KURAC")
  }

//  private def reachableFields(matrix: Matrix): mutable.Set[Int] = {
//    def _reachableFields(position: Int, discovered: mutable.Set[Int]): Unit = {
//      discovered += position
//
//      // Add boxes
//      matrix.getNeighbours(matrix.linearizedIndexToPosition(position))
//        .map(matrix.positionToLinearizedIndex)
//        .filter(isBox(matrix, _))
//        .foreach(discovered.+=)
//
//      matrix.getNeighbours(matrix.linearizedIndexToPosition(position))
//        .map(matrix.positionToLinearizedIndex)
//        .filter(isFieldStepable(matrix, _))
//        .filterNot(discovered)
//        .foreach(_reachableFields(_, discovered))
//    }
//
//    val player = matrix.positionToLinearizedIndex(matrix.playerPosition)
//    val discovered = mutable.Set[Int]() += player
//
//    _reachableFields(player, discovered)
//    discovered
//  }
//
//  private def validPushes(matrix: Matrix, boxIndex: Int): List[Direction] = {
//
//    val reachable = reachableFields(matrix)
//
//
//    val position = matrix.linearizedIndexToPosition(boxIndex)
//    Directions.sideDirections.filter(direction => {
//      matrix.isMoveLegal(position, direction, 1)
//        && isFieldStepable(matrix, position + matrix.directionOffset(oppositeDirection(direction)))
//    })
//  }

  private def move(matrix: Matrix, direction: Direction): Matrix = {
    val ret = matrix.copy
    val player = ret.playerPosition
    val oldBox = player + matrix.directionOffset(direction)
    val newBox = oldBox + matrix.directionOffset(direction)

    updateField(ret, player, FieldTypes.EMPTY)   // clear player
    updateField(ret, oldBox, FieldTypes.PLAYER)  // set player where box was
    updateField(ret, newBox, FieldTypes.BOX)     // move box

    ret
  }

  private def updateField(matrix: Matrix, position: Position, newFieldType: FieldType): Unit = {
    val combinedField = matrix.combinedFieldType(matrix.get(position), newFieldType)
    matrix.update(position, combinedField)
  }

  private def oppositeDirection(direction: Direction) = direction match
    case Directions.UP => Directions.DOWN
    case Directions.DOWN => Directions.UP
    case Directions.RIGHT => Directions.LEFT
    case Directions.LEFT => Directions.RIGHT


  private def isFieldStepable(matrix: Matrix, position: Position): Boolean = isFieldStepable(matrix, matrix.positionToLinearizedIndex(position))
  private def isFieldStepable(matrix: Matrix, index: Int): Boolean = matrix.get(index) match
    case FieldTypes.EMPTY | FieldTypes.PLAYER | FieldTypes.PLAYER_ON_GOAL | FieldTypes.GOAL => true
    case _ => false

  def isBox(matrix: Matrix, position: Position): Boolean = isBox(matrix, matrix.positionToLinearizedIndex(position))
  def isBox(matrix: Matrix, index: Int): Boolean =
    matrix.get(index) == FieldTypes.BOX || matrix.get(index) == FieldTypes.BOX_ON_GOAL

}
