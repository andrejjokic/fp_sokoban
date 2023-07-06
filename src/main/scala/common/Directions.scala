package sokoban
package common

object Directions extends Enumeration {
  type Direction = Value
  val UP, DOWN, LEFT, RIGHT, UP_RIGHT, DOWN_RIGHT, UP_LEFT, DOWN_LEFT = Value

  val sideDirections: List[Direction] = List(UP, DOWN, LEFT, RIGHT)
}
