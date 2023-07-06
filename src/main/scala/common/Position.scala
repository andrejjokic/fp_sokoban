package sokoban
package common

class Position(val row: Int, val col: Int) {
  def +(b: Position) = new Position(row + b.row, col + b.col)

  override def equals(obj: Any): Boolean = obj match
    case that: Position => row == that.row && col == that.col
    case _ => false


  override def toString = s"[$row,$col]"
}
