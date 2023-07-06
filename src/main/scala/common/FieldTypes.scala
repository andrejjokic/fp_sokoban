package sokoban
package common

object FieldTypes extends Enumeration {
  type FieldType = Value
  val PLAYER, GOAL, BOX, WALL, EMPTY, BOX_ON_GOAL, PLAYER_ON_GOAL = Value
}
