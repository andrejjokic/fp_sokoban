package sokoban
package operations

import model.Matrix

import sokoban.common.Position

class Fractalization(val row: Int, val col: Int) extends Operation {
  override def apply(v1: Matrix): Matrix = {
    val ret = v1.copy
    ret.fractializeField(new Position(row, col))
    ret
  }
}
