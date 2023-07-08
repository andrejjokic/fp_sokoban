package sokoban
package operations

import sokoban.model.Matrix

import scala.annotation.tailrec

class OperationSequence(val operations: List[Operation]) extends Operation {

  def this(operation: Operation) = this(List(operation))
  def this() = this(Nil)

  override def apply(v1: Matrix): Matrix = {
    def _apply(matrix: Matrix, ops: List[Operation]): Matrix = {
      try {
        ops match
          case Nil => matrix
          case h :: t => _apply(h(matrix), t)
      }
      catch
        case e: OperationException => throw e
        case _ => throw new OperationException(matrix)
    }

    _apply(v1, operations.reverse)
  }

  def append(g: Operation): OperationSequence = {
    new OperationSequence(g::operations)
  }
}
