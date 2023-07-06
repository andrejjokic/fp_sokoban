package sokoban
package model

import sokoban.common.Directions.Direction
import sokoban.common.FieldTypes.FieldType
import sokoban.common.{Directions, FieldTypes, Position}
import sokoban.utils.Converter

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

class Matrix {

  type Array2D = Array[Array[FieldType]]

  private var matrix: Array2D = create(0, 0)
  private val movesPlayed = new mutable.Stack[MovePlayed]

  private def create(rows: Int, columns: Int) = Array.ofDim[FieldType](rows, columns)

  def init(board: List[List[Char]]): Unit = {
    val rows = for (row <- board) yield row.map(Converter.toFieldType).toArray
    matrix = rows.toArray
  }

  private def init(board: Array[Array[FieldType]]): Matrix = {
    for (i <- board.indices; j <- board(i).indices) update(i, j, board(i)(j))
    this
  }

  def get(row: Int, column: Int): FieldType = {
    require(row < matrix.length)
    require(column < matrix(row).length)

    matrix(row)(column)
  }

  def get(position: Position): FieldType = {
    get(position.row, position.col)
  }

  def get(linearizedIndex: Int): FieldType = {
    get(linearizedIndexToPosition(linearizedIndex))
  }

  def update(row: Int, col: Int, value: FieldType): Unit = {
    require(row < matrix.length)
    require(col < matrix(row).length)

    matrix(row)(col) = value
  }

  def update(position: Position, value: FieldType): Unit = {
    update(position.row, position.col, value)
  }

  def update(index: Int, value: FieldType): Unit = {
    update(linearizedIndexToPosition(index), value)
  }

  def restoreField(fieldSnapshot: FieldSnapshot): Unit = update(fieldSnapshot.position, fieldSnapshot.fieldType)

  def linearize: List[FieldType] = {
    linearizedPositions.map(get)
  }

  private def linearizedPositions: List[Position] = {
    val linearized = for (i <- matrix.indices; j <- matrix(i).indices) yield new Position(i, j)
    linearized.toList
  }
  private def linearizedIndex(row: Int, col: Int) = row * columns + col
  def positionToLinearizedIndex(position: Position) = linearizedIndex(position.row, position.col)
  def linearizedIndexToPosition(index: Int) = new Position(index / columns, index % columns)

  def rows: Int = matrix.length
  def columns: Int = matrix.length match
    case 0 => 0
    case _ => matrix(0).length

  def map(f: (FieldType => FieldType)): Array2D = {
    matrix.map(row => row.map(f))
  }

  private def isInBounds(position: Position): Boolean =
    position.row >= 0
      && position.row < rows
      && position.col >= 0
      && position.col < columns

  private def isNotInBounds(position: Position): Boolean = !isInBounds(position)

  def playerPosition: Position = {
    for (i <- matrix.indices; j <- matrix(i).indices
         if matrix(i)(j) == FieldTypes.PLAYER || matrix(i)(j) == FieldTypes.PLAYER_ON_GOAL)
      return new Position(i, j)
    throw new NoSuchElementException
  }

  def movePlayer(direction: Direction): Unit = {
    if (isMoveLegal(direction))
      makeMove(direction)
  }

  def isMoveLegal(direction: Directions.Direction): Boolean = isMoveLegal(playerPosition, direction)

  def isMoveLegal(position: Position, direction: Direction, startFieldsMovedCnt: Int = 0): Boolean = {
    val dirOffset = directionOffset(direction)

    @tailrec
    def _isMoveLegal(position: Position, fieldsMoved: Int): Boolean = {
      val fieldType = get(position)
      fieldType match
        case FieldTypes.WALL => false
        case FieldTypes.EMPTY | FieldTypes.GOAL => true
        case FieldTypes.BOX | FieldTypes.BOX_ON_GOAL if fieldsMoved >= 2 => false
        case FieldTypes.BOX | FieldTypes.BOX_ON_GOAL if fieldsMoved < 2 => _isMoveLegal(position + dirOffset, fieldsMoved + 1)
        case FieldTypes.PLAYER | FieldTypes.PLAYER_ON_GOAL  => _isMoveLegal(position + dirOffset, fieldsMoved + 1)
    }

    _isMoveLegal(position, startFieldsMovedCnt)
  }

  private def makeMove(direction: Direction): Unit = {
    val dirOffset = directionOffset(direction)
    val move = new MovePlayed

    def _makeMove(position: Position, newFieldType: FieldType): Unit = {
      val oldFieldType = get(position)

      oldFieldType match {
        case FieldTypes.EMPTY | FieldTypes.GOAL => /* no action */
        case _ => _makeMove(position + dirOffset, oldFieldType)
      }

      update(position, combinedFieldType(oldFieldType, newFieldType))
      move += new FieldSnapshot(position, oldFieldType)
    }

    val swapPlayerField = get(playerPosition) match
      case FieldTypes.PLAYER => FieldTypes.EMPTY
      case FieldTypes.PLAYER_ON_GOAL => FieldTypes.GOAL

    _makeMove(playerPosition, swapPlayerField)
    movesPlayed.push(move)
  }

  def combinedFieldType(oldField: FieldType, newField: FieldType): FieldType = {
    (oldField, newField) match
      case (FieldTypes.EMPTY, FieldTypes.PLAYER | FieldTypes.PLAYER_ON_GOAL) => FieldTypes.PLAYER
      case (FieldTypes.EMPTY, FieldTypes.BOX | FieldTypes.BOX_ON_GOAL) => FieldTypes.BOX
      case (FieldTypes.GOAL, FieldTypes.PLAYER | FieldTypes.PLAYER_ON_GOAL) => FieldTypes.PLAYER_ON_GOAL
      case (FieldTypes.GOAL, FieldTypes.BOX | FieldTypes.BOX_ON_GOAL) => FieldTypes.BOX_ON_GOAL
      case (FieldTypes.BOX, FieldTypes.PLAYER | FieldTypes.PLAYER_ON_GOAL) => FieldTypes.PLAYER
      case (FieldTypes.BOX_ON_GOAL, FieldTypes.PLAYER | FieldTypes.PLAYER_ON_GOAL) => FieldTypes.PLAYER_ON_GOAL
      case (_, FieldTypes.EMPTY) => FieldTypes.EMPTY
      case (_, FieldTypes.GOAL) => FieldTypes.GOAL
      case (_, _) => throw new UnsupportedOperationException()
  }

  def directionOffset(direction: Direction): Position = direction match
    case Directions.UP => new Position(-1, 0)
    case Directions.DOWN => new Position(+1, 0)
    case Directions.LEFT => new Position(0, -1)
    case Directions.RIGHT => new Position(0, +1)
    case Directions.UP_RIGHT => new Position(-1, +1)
    case Directions.UP_LEFT => new Position(-1, -1)
    case Directions.DOWN_RIGHT => new Position(+1, +1)
    case Directions.DOWN_LEFT => new Position(+1, -1)

  def isGameCompleted: Boolean = linearize.count(field => field == FieldTypes.BOX) == 0

  def undoMove(): Unit = {
    if (movesPlayed.nonEmpty)
      movesPlayed.pop().getFieldSnapshots.foreach(restoreField)
  }

  private val defaultField = FieldTypes.EMPTY
  private def emptyArray(size: Int) = Array.ofDim[FieldType](size).map(_ => defaultField)
  private def emptyRow = emptyArray(columns)
  private def emptyColumn = emptyArray(rows)
  private def addToArray[T: ClassTag](array: Array[T], value: T, asFirst: Boolean): Array[T] = if (asFirst) {
    value +: array
  } else {
    array :+ value
  }
  private def dropFromArray[T: ClassTag](array: Array[T], isFirst: Boolean): Array[T] = if (isFirst) {
    array.drop(1)
  } else {
    array.dropRight(1)
  }

  def addRow(asFirst: Boolean): Unit = {
    matrix = addToArray(matrix, emptyRow, asFirst)
  }
  def addColumn(asFirst: Boolean): Unit = {
    matrix = matrix.map(row => addToArray(row, defaultField, asFirst))
  }
  def removeRow(isFirst: Boolean): Unit = {
    matrix = dropFromArray(matrix, isFirst)
  }
  def removeColumn(isFirst: Boolean): Unit = {
    matrix = matrix.map(row => dropFromArray(row, isFirst))
  }

  def swapBoxAndGoals(): Unit = {
    def inverse(fieldType: FieldType): FieldType = fieldType match
      case FieldTypes.BOX => FieldTypes.GOAL
      case FieldTypes.GOAL => FieldTypes.BOX
      case field => field

    matrix = map(inverse)
  }

  def filterField(row: Int, col: Int, n: Int): Unit = {
    val fieldPosition = new Position(row, col)
    if (getNeighbours(fieldPosition, n).count(isWall) > 0)
      update(fieldPosition, FieldTypes.EMPTY)
  }

  def getNeighbours(position: Position, maxOffset: Int = 1): List[Position] =
    def move(offset: Position) = (offset.row, offset.col) match
      case (0, col) => new Position(0, col + java.lang.Integer.signum(col))
      case (row, 0) => new Position(row + java.lang.Integer.signum(row), 0)

    def multiply(offset: Position, n: Int): List[Position] = n match
      case 0 => Nil
      case n => offset :: multiply(move(offset), n - 1)

    Directions.sideDirections
      .map(directionOffset)
      .flatMap(offset => multiply(offset, maxOffset))
      .map(offset => position + offset)
      .filter(isInBounds)

  private def getSurroundingPositions(position: Position): List[Position] = {
    Directions.values.toList
      .map(directionOffset)
      .map(offset => position + offset)
  }

  private def isWall(position: Position): Boolean = {
    if (isNotInBounds(position)) false
    else get(position) == FieldTypes.WALL
  }
  private def isNotWall(position: Position): Boolean = !isWall(position)
  private def isNotWall(index: Int): Boolean = isNotWall(linearizedIndexToPosition(index))

  private def getQueue(startPosition: Position) = new mutable.Queue[Position]().enqueue(startPosition)
  private def getSet(startPosition: Position) = mutable.Set[Int]() += positionToLinearizedIndex(startPosition)

  def minimizeWalls(): Unit = {
    def wallToDefault(fieldType: FieldType): FieldType = fieldType match
      case FieldTypes.WALL => defaultField
      case other => other

    def updateWalls(walls: List[Position]): Unit = {
      matrix = map(wallToDefault)
      walls.foreach(position => update(position, FieldTypes.WALL))
    }

    if (!isSurroundedByWalls) return
    updateWalls(outerWalls)
    updateWalls(reachableWalls)
  }

  private def reachableWalls: List[Position] = {
    reachableFields.toList.map(linearizedIndexToPosition).filter(isWall)
  }

  private def innerFields: mutable.Set[Int] = {
    reachableFields.filter(isNotWall)
  }

  def reachableFields: mutable.Set[Int] = {
    val discovered = getSet(playerPosition)
    val queue = getQueue(playerPosition)

    _isSurroundedByWalls(queue, discovered)
    discovered
  }

  @tailrec
  private def _isSurroundedByWalls(queue: mutable.Queue[Position], discovered: mutable.Set[Int]): Boolean = {
    if (queue.isEmpty) return true
    val currentPosition = queue.dequeue()
    if (isNotInBounds(currentPosition)) return false

    getSurroundingPositions(currentPosition)
      .filter(isNotWall)
      .map(positionToLinearizedIndex)
      .filterNot(discovered)
      .foreach(neighbour => {
        discovered += neighbour
        queue.enqueue(linearizedIndexToPosition(neighbour))
      })

    // Just add walls to discovered
    getSurroundingPositions(currentPosition)
      .filter(isWall)
      .map(positionToLinearizedIndex)
      .foreach(discovered.+=)

    _isSurroundedByWalls(queue, discovered)
  }

  private def isSurroundedByWalls: Boolean = {
    val discovered = getSet(playerPosition)
    val queue = getQueue(playerPosition)

    _isSurroundedByWalls(queue, discovered)
  }

  private def isOnEdge(position: Position): Boolean =
    position.row == 0 || position.col == 0 || position.row == rows - 1 || position.col == columns - 1

  private def outerWalls: List[Position] = {
    def isInteresting(currentPosition: Position, neighbour: Position): Boolean = get(currentPosition) match
      case FieldTypes.WALL => isOnEdge(neighbour)
      case _ => true

    val start = new Position(0, 0)
    val discovered = getSet(start)
    val queue = getQueue(start)

    @tailrec
    def _outerWalls(): Unit = {
      if (queue.isEmpty) return
      val current = queue.dequeue()

      getSurroundingPositions(current)
        .filter(isInBounds)
        .filter(neighbour => isInteresting(current, neighbour))
        .map(positionToLinearizedIndex)
        .filterNot(discovered)
        .foreach(neighbour => {
          discovered += neighbour
          queue.enqueue(linearizedIndexToPosition(neighbour))
        })

      _outerWalls()
    }

    _outerWalls()
    discovered.toList
      .map(linearizedIndexToPosition)
      .filter(isWall)
  }

  def fractializeField(position: Position): Unit = {
    def extendMapForNeighbours(): Position = {
      var row = position.row
      var col = position.col

      if (getSurroundingPositions(position).count(_.row < 0) > 0) {
        addRow(true)
        row += 1
      }

      if (getSurroundingPositions(position).count(_.row >= rows) > 0)
        addRow(false)

      if (getSurroundingPositions(position).count(_.col < 0) > 0) {
        addColumn(true)
        col += 1
      }

      if (getSurroundingPositions(position).count(_.col >= columns) > 0)
        addColumn(false)

      new Position(row, col)
    }

    if (isNotWall(position)) return

    // Extend map for every out of bounds field
    val newPosition = extendMapForNeighbours()

    val inner = innerFields
    update(newPosition, FieldTypes.EMPTY)

    // Surround the field with walls
    getSurroundingPositions(newPosition)
      .map(positionToLinearizedIndex)
      .filterNot(inner)
      .foreach(index => update(index, FieldTypes.WALL))

    // Minimize walls
    minimizeWalls()
  }

  private def doesNumberOfBoxesAndGoalsMatch: Boolean = {
    val reachable = reachableFields.toList.map(get)
    reachable.count(_.==(FieldTypes.BOX))
      == reachable.count(position => position == FieldTypes.GOAL || position == FieldTypes.PLAYER_ON_GOAL)
  }

  private def isOnlyOnePlayerPresent: Boolean = {
    linearize.count(field => field == FieldTypes.PLAYER || field == FieldTypes.PLAYER_ON_GOAL) == 1
  }

  val MAP_IS_CORRECT = "Map is correct."

  def checkValidity: String = {
    if (!isOnlyOnePlayerPresent) "Map does not contain exactly 1 player."
    else if (!isSurroundedByWalls) "Player is not correctly surrounded by walls."
    else if (!doesNumberOfBoxesAndGoalsMatch) "Number of boxes and goals do not match."
    else MAP_IS_CORRECT
  }

  def copy: Matrix = {
    new Matrix().init(this.matrix)
  }
}
