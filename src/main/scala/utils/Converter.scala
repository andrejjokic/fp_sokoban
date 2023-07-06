package sokoban
package utils

import scala.collection.mutable
import view.{Field, FieldImage}

import sokoban.common
import sokoban.common.{Directions, FieldTypes}
import sokoban.common.FieldTypes.FieldType
import sokoban.common.Directions.Direction

object Converter {
  private val fieldTypeToField = mutable.HashMap(
    FieldTypes.EMPTY -> FieldImage.empty,
    FieldTypes.WALL -> FieldImage.wall,
    FieldTypes.GOAL -> FieldImage.goal,
    FieldTypes.BOX -> FieldImage.box,
    FieldTypes.PLAYER -> FieldImage.player,
    FieldTypes.BOX_ON_GOAL -> FieldImage.boxOnGoal,
    FieldTypes.PLAYER_ON_GOAL -> FieldImage.player
  )

  private val charToFieldType = mutable.HashMap(
    '-' -> FieldTypes.EMPTY,
    '#' -> FieldTypes.WALL,
    '.' -> FieldTypes.GOAL,
    'X' -> FieldTypes.BOX,
    'S' -> FieldTypes.PLAYER,
    'O' -> FieldTypes.BOX_ON_GOAL
  )

  private val charToDirection = mutable.HashMap(
    'L' -> Directions.LEFT,
    'D' -> Directions.DOWN,
    'R' -> Directions.RIGHT,
    'U' -> Directions.UP
  )

  def toField(fieldType: FieldType): Field = new Field(fieldTypeToField.getOrElse(fieldType, FieldImage.empty))
  def toFieldType(c: Char): FieldType = charToFieldType.getOrElse(c, FieldTypes.EMPTY)
  def toDirection(c: Char): Direction = charToDirection.getOrElse(c, Directions.UP)
}
