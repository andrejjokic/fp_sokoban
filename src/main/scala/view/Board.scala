package sokoban
package view

import model.Matrix

import sokoban.utils.Converter

import scala.swing.GridPanel
import scala.swing.event.KeyPressed
import scala.swing.event.KeyEvent

class Board(matrix: Matrix) extends GridPanel(matrix.rows, matrix.columns) {
    contents ++= matrix.linearize.map(Converter.toField)
}
