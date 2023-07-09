package sokoban
package solver

import sokoban.common.Directions.Direction
import sokoban.common.{Directions, FieldTypes, Position}
import sokoban.model.Matrix

import scala.annotation.tailrec
import scala.collection.mutable

class MatrixSnapshot(val matrix: Matrix, val parent: Option[MatrixSnapshot], val parentDirection: Option[Direction])
  extends Ordered[MatrixSnapshot] {

  def this(matrix: Matrix, parent: MatrixSnapshot, parentDirection: Direction) =
    this(matrix, Some(parent), Some(parentDirection))

  private val player = matrix.playerPosition
  private val boxes = matrix.linearizedPositions.filter(isBox(matrix, _))
  private val goals = matrix.linearizedPositions.filter(isGoal(matrix, _))
  private val stringRepresentation = toString

  override def hashCode(): Int = stringRepresentation.hashCode

  override def equals(obj: Any): Boolean = obj match
    case that: MatrixSnapshot => stringRepresentation.equals(that.stringRepresentation)
    case _ => false

  private def isBox(matrix: Matrix, position: Position): Boolean =
    matrix.get(position) == FieldTypes.BOX || matrix.get(position) == FieldTypes.BOX_ON_GOAL

  override def toString: String = {
    val ret = new StringBuffer()
    ret.append(player.toString)
    boxes.sortBy(pos => (pos.row, pos.col)).foreach(pos => {
      ret.append(",")
      ret.append(pos.toString)
    })
    ret.toString
  }

  private def isGoal(matrix: Matrix, position: Position): Boolean =
    matrix.get(position) == FieldTypes.GOAL || matrix.get(position) == FieldTypes.PLAYER_ON_GOAL

  private def isBoxNotOnGoal(position: Position): Boolean = matrix.get(position) == FieldTypes.BOX

  private def manhattanDistance(p1: Position, p2: Position): Int =
    (p1.row - p2.row).abs + (p1.col - p2.col).abs

  private def distanceToClosestGoal(box: Position): Int = {
    goals.map(manhattanDistance(_, box)).min
  }

  private val boxesNotOnGoalsPenalty: Int =
    boxes.count(isBoxNotOnGoal) * 10

  private val boxesToClosestGoalPenalty: Int =
    boxes.filter(isBoxNotOnGoal).map(distanceToClosestGoal).sum

  private val cost: Int = boxesNotOnGoalsPenalty + boxesToClosestGoalPenalty

  private val cost2: Int = {
    def distanceToNearestGoal(box: Position): Int = {
      goals.map(distance(box, _)).min
    }

    boxes.filter(isBoxNotOnGoal).map(distanceToClosestGoal).sum
  }

  private def distance(start: Position, end: Position): Int = {
    class PositionDistance(val position: Position, val distance: Int) {}

    val visited = mutable.Set[Position]()
    val queue = mutable.Queue[PositionDistance]()

    @tailrec
    def _distance(): Int = {
      if (queue.isEmpty) return Int.MaxValue
      val current = queue.dequeue()
      if (current == end) return current.distance

      matrix.getNeighbours(current.position)
        .filter(matrix.isNotWall)
        .filterNot(visited)
        .foreach(neighbour => {
          queue.enqueue(new PositionDistance(neighbour, current.distance + 1))
          visited += neighbour
        })

      _distance()
    }

    queue.enqueue(new PositionDistance(start, 0))
    visited += start

    _distance()
  }

  override def compare(that: MatrixSnapshot): Int = -this.cost.compareTo(that.cost)
}
