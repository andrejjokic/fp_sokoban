package sokoban
package model

import scala.collection.mutable.ListBuffer

class MovePlayed {
  private val fieldSnapshots = new ListBuffer[FieldSnapshot]

  def +=(fieldSnapshot: FieldSnapshot): Unit = fieldSnapshots += fieldSnapshot
  def getFieldSnapshots: List[FieldSnapshot] = fieldSnapshots.toList
}
