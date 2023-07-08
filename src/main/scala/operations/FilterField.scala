package sokoban
package operations

import model.Matrix

class FilterField(val row: Int, val col: Int, val distance: Int) extends Operation {
  override def apply(v1: Matrix): Matrix = {
    val ret = v1.copy
    ret.filterField(row, col, distance)
    throw new IllegalArgumentException()
    ret
  }
}
