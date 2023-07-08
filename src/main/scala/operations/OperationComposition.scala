package sokoban
package operations

import model.Matrix

import scala.annotation.tailrec

class OperationComposition(val operations: List[Operation]) extends Operation {

  def this(operation: Operation) = this(List(operation))
  def this() = this(Nil)

  override def apply(v1: Matrix): Matrix = {
    @tailrec
    def _apply(matrix: Matrix, ops: List[Operation]): Matrix = ops match
      case Nil => matrix
      case h :: t => _apply(h(matrix), t)

    try
      _apply(v1, operations.reverse)
    catch
      case _ => throw new OperationException(v1)
  }

  def append(g: Operation): OperationComposition = {
    new OperationComposition(g::operations)
  }
}
