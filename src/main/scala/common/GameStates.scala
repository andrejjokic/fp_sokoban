package sokoban
package common

object GameStates extends Enumeration {
  type GameState = Value
  val UNINITIALIZED, MAP_LOADED, PLAYING, FINISHED = Value
}
