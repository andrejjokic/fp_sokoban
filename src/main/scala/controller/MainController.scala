package sokoban
package controller

import model.Matrix
import utils.{Converter, IOParser}

import sokoban.common.Directions.Direction
import sokoban.common.FieldTypes.FieldType
import sokoban.common.GameStates.GameState
import sokoban.common.{Directions, GameStates, Position}
import sokoban.solver.Solver
import sokoban.view.{Board, EmptyBoard}

import scala.swing.{Component, Frame, Panel}

class MainController() {
  val matrix = new Matrix
  var gameState: GameState = GameStates.UNINITIALIZED

  def loadMap(filePath: String): Component = {
    matrix.init(IOParser.read(filePath))
    gameState = GameStates.MAP_LOADED
    new Board(matrix)
  }

  def startGame(): Unit = {
    gameState = GameStates.PLAYING
  }

  def move(direction: Direction): Component = {
    def movePlayer(direction: Direction): Component = {
      matrix.movePlayer(direction)
      if (matrix.isGameCompleted) gameState = GameStates.FINISHED
      new Board(matrix)
    }

    gameState match
      case GameStates.UNINITIALIZED => EmptyBoard
      case GameStates.PLAYING => movePlayer(direction)
      case _ => new Board(matrix)

  }

  def undoMove(): Component = {
    gameState match
      case GameStates.UNINITIALIZED => EmptyBoard
      case GameStates.PLAYING =>
        matrix.undoMove()
        new Board(matrix)
      case _ => new Board(matrix)
  }

  def playSequenceOfMoves(input: String): Component = {
    def _playSequenceOfMoves(input: String): Component = {
      IOParser.readCharPerLine(input).map(Converter.toDirection).foreach(dir => move(dir))
      new Board(matrix)
    }

    gameState match
      case GameStates.UNINITIALIZED => EmptyBoard
      case GameStates.PLAYING => _playSequenceOfMoves(input)
      case _ => new Board(matrix)
  }

  def addRowOrColumn(isRow: Boolean, asFirst: Boolean): Component = {
    (gameState, isRow) match
      case (GameStates.MAP_LOADED, true) =>
        matrix.addRow(asFirst)
        new Board(matrix)
      case (GameStates.MAP_LOADED, false) =>
        matrix.addColumn(asFirst)
        new Board(matrix)
      case (_, _) => EmptyBoard
  }

  def removeRowOrColumn(isRow: Boolean, isFirst: Boolean): Component = {
    (gameState, isRow) match
      case (GameStates.MAP_LOADED, true) =>
        matrix.removeRow(isFirst)
        new Board(matrix)
      case (GameStates.MAP_LOADED, false) =>
        matrix.removeColumn(isFirst)
        new Board(matrix)
      case (_, _) => EmptyBoard
  }

  def editFieldType(row: Int, col: Int, newFieldType: FieldType): Component = {
    gameState match
      case GameStates.MAP_LOADED =>
        matrix.update(row, col, newFieldType)
        new Board(matrix)
      case _ => EmptyBoard
  }

  def swapBoxAndGoals(): Component = {
    gameState match
      case GameStates.MAP_LOADED =>
        matrix.swapBoxAndGoals()
        new Board(matrix)
      case _ => EmptyBoard
  }

  def minimizeWalls(): Component = {
    gameState match
      case GameStates.MAP_LOADED =>
        matrix.minimizeWalls()
        new Board(matrix)
      case _ => EmptyBoard
  }

  def filterField(row: Int, col: Int, n: Int): Component = {
    gameState match
      case GameStates.MAP_LOADED =>
        matrix.filterField(row, col, n)
        new Board(matrix)
      case _ => EmptyBoard
  }

  def fractializeField(row: Int, col: Int): Component = {
    gameState match
      case GameStates.MAP_LOADED =>
        matrix.fractializeField(new Position(row, col))
        new Board(matrix)
      case _ => EmptyBoard
  }

  def checkMapValidity: String = {
    gameState match
      case GameStates.MAP_LOADED => matrix.checkValidity
      case _ => "Map is not loaded."
  }

  def solve(): Unit = {
    gameState match
      case GameStates.MAP_LOADED | GameStates.PLAYING => Solver.solve(matrix)
      case _ => /* nothing */
  }
}
