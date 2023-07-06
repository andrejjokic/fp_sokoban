package sokoban
package solver

import sokoban.common.{FieldTypes, Position}
import sokoban.model.Matrix

class MatrixSnapshot(matrix: Matrix) {

  private val player = matrix.playerPosition
  private val boxes = matrix.linearizedPositions.filter(isBox(matrix, _))

  override def hashCode(): Int = toString.hashCode

  override def equals(obj: Any): Boolean = obj match
    case that: MatrixSnapshot => player == that.player && compareBoxes(that.boxes)
    case _ => false

  private def compareBoxes(otherBoxes: List[Position]): Boolean = {
    if (boxes.size != otherBoxes.size) return false
    boxes.foreach(box => {
      if (!otherBoxes.exists(_.==(box))) return false
    })
    true
  }

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
