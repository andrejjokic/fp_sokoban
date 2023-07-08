package sokoban
package operations

import model.Matrix

class MinimizeWalls extends Operation {
  override def apply(v1: Matrix): Matrix = {
    val ret = v1.copy
    ret.minimizeWalls()
    ret
  }
}
