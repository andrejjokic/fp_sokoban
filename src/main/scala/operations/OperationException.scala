package sokoban
package operations

import model.Matrix

class OperationException(val matrix: Matrix) extends Throwable {
}
