package sokoban
package solver

import sokoban.common.{FieldTypes, Position}
import sokoban.model.Matrix

class MatrixSnapshot(matrix: Matrix) {

  private val player = matrix.playerPosition
  private val boxes = matrix.linearizedPositions.filter(isBox(matrix, _))
  private val stringRepresentation = toString

  override def hashCode(): Int = stringRepresentation.hashCode

  override def equals(obj: Any): Boolean = obj match
    case that: MatrixSnapshot => stringRepresentation.equals(that.stringRepresentation)
    case _ => false

  private def isBox(matrix: Matrix, position: Position): Boolean =
    matrix.get(position) == FieldTypes.BOX || matrix.get(position) == FieldTypes.BOX_ON_GOAL

  override def toString: String = {
    val ret = new StringBuffer()
    ret.append(player.toString)
    boxes.sortBy(pos => (pos.row, pos.col)).foreach(pos => {
      ret.append(",")
      ret.append(pos.toString)
    })
    ret.toString
  }
}
