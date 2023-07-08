package sokoban

import scala.swing.{event, *}
import controller.MainController
import view.Board

import sokoban.App.controller
import sokoban.common.{Directions, FieldTypes, GameStates, Position}
import sokoban.common.FieldTypes.FieldType
import sokoban.operations.{FilterField, Fractalization, Inversion, MinimizeWalls, OperationComposition, OperationSequence}
import sokoban.utils.{Converter, IOParser}

import scala.collection.mutable
import scala.swing.event.{ButtonClicked, Key, KeyPressed, MouseClicked}

object App extends SimpleSwingApplication {
  private val controller = new MainController

  val top: Frame = new MainFrame {
    mainFrame =>

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem("Start game") {
          reactions += {
            case ButtonClicked(_) => controller.startGame()
          }
        }
      }

      contents += new Menu("Tools") {
        contents += new MenuItem("Undo move") {
          reactions += {
            case ButtonClicked(_) => mainFrame.contents = controller.undoMove()
          }
        }

        contents += new MenuItem("Load moves") {
          reactions += {
            case ButtonClicked(_) => showOpenDialog() match
              case null =>
              case input: String => playSequenceOfMoves(input)
          }
        }

        contents += new MenuItem("Get solution") {
          reactions += {
            case ButtonClicked(_) => Dialog.showMessage(this, controller.solve(), "Solution")
          }
        }
      }

      contents += new Menu("Map") {
        contents += new MenuItem("Load") {
          reactions += {
            case ButtonClicked(_) => showOpenDialog() match
              case null =>
              case input: String =>
                mainFrame.contents = controller.loadMap(input)
          }
        }

        contents += new Menu("Edit") {
          contents += new MenuItem("Field") {
            reactions += {
              case ButtonClicked(_) => showEditFieldDialog()
            }
          }
          contents += new MenuItem("Add row/column") {
            reactions += {
              case ButtonClicked(_) =>
                showEditRowOrColumnNumberDialog("Add row/column")(controller.addRowOrColumn)
            }
          }
          contents += new MenuItem("Remove row/column") {
            reactions += {
              case ButtonClicked(_) =>
                showEditRowOrColumnNumberDialog("Remove row/column")(controller.removeRowOrColumn)
            }
          }
          contents += new MenuItem("Swap box and goals") {
            reactions += {
              case ButtonClicked(_) => top.contents = controller.swapBoxAndGoals()
            }
          }
          contents += new MenuItem("Minimize walls") {
            reactions += {
              case ButtonClicked(_) => top.contents = controller.minimizeWalls()
            }
          }
          contents += new MenuItem("Filter field") {
            reactions += {
              case ButtonClicked(_) => showFilterFieldDialog()
            }
          }

          contents += new MenuItem("Fractialize field") {
            reactions += {
              case ButtonClicked(_) => showFractializeFieldDialog()
            }
          }
        }
        contents += new MenuItem("Check validity") {
          reactions += {
            case ButtonClicked(_) => Dialog.showMessage(this, controller.checkMapValidity, "Map validity")
          }
        }
      }
    }

    addKeyListeners()

    title = "Sokoban"
    size = new Dimension(800, 500)
    centerOnScreen()
  }

  def addKeyListeners(): Unit = {
    import java.awt.KeyboardFocusManager
    import java.awt.KeyEventDispatcher
    import java.awt.event.KeyEvent

    class MyDispatcher extends KeyEventDispatcher {
      override def dispatchKeyEvent(e: KeyEvent): Boolean = {
        if (e.getID == KeyEvent.KEY_PRESSED) {
          e.getKeyCode match
            case KeyEvent.VK_W => top.contents = controller.move(Directions.UP)
            case KeyEvent.VK_A => top.contents = controller.move(Directions.LEFT)
            case KeyEvent.VK_S => top.contents = controller.move(Directions.DOWN)
            case KeyEvent.VK_D => top.contents = controller.move(Directions.RIGHT)
            case _ =>
        }
        false
      }
    }

    KeyboardFocusManager.getCurrentKeyboardFocusManager.addKeyEventDispatcher(new MyDispatcher);
  }

  private def showOpenDialog(): String = {
    val fileChooser = new FileChooser()
    fileChooser.showOpenDialog(this.top) match
      case FileChooser.Result.Approve => fileChooser.selectedFile.toString
      case _ => null
  }

  private def showEditRowOrColumnNumberDialog(name: String)(endpoint: (Boolean, Boolean) => Component): Unit = {
    new Dialog {
      contents = new GridPanel(3, 1) {
        val rowButton = new RadioButton("Row")
        val colButton = new RadioButton("Column")
        val rowOrColGroup = new ButtonGroup {
          buttons ++= List(rowButton, colButton)
          select(rowButton)
        }
        val firstButton = new RadioButton("First")
        val lastButton = new RadioButton("Last")
        val firstOrLastGroup = new ButtonGroup {
          buttons ++= List(firstButton, lastButton)
          select(lastButton)
        }

        contents += new BoxPanel(Orientation.Horizontal) {
          contents ++= rowOrColGroup.buttons
        }
        contents += new BoxPanel(Orientation.Horizontal) {
          contents ++= firstOrLastGroup.buttons
        }
        contents += new Button("Done") {
          reactions += {
            case ButtonClicked(_) =>
              top.contents = endpoint(
                rowOrColGroup.selected.get == `rowButton`,
                firstOrLastGroup.selected.get == `firstButton`)
              close()
          }
        }
      }
      title = name
      centerOnScreen()
      size = new Dimension(250, 150)
      open()
    }
  }

  private def showEditFieldDialog(): Unit = {
    new Dialog {
      contents = new GridPanel(4, 1) {
        val row = new TextField("0")
        val col = new TextField("0")
        val fieldType = new ComboBox[FieldType](FieldTypes.values.toList) {
          selection.item = FieldTypes.EMPTY
        }

        contents += new GridPanel(1, 2) {
          contents += new Label("Row:")
          contents += row
        }
        contents += new GridPanel(1, 2) {
          contents += new Label("Column:")
          contents += col
        }
        contents += new GridPanel(1, 2) {
          contents += new Label("New field type:")
          contents += fieldType
        }
        contents += new Button("Edit") {
          reactions += {
            case ButtonClicked(_) =>
              top.contents = controller.editFieldType(row.text.toInt, col.text.toInt, fieldType.selection.item)
              close()
          }
        }
      }
      title = "Edit field"
      centerOnScreen()
      size = new Dimension(300, 200)
      open()
    }
  }

  private def showFilterFieldDialog(): Unit = {
    new Dialog {
      contents = new GridPanel(4, 1) {
        val row = new TextField("0")
        val col = new TextField("0")
        val distance = new TextField("1")

        contents += new GridPanel(1, 2) {
          contents += new Label("Row:")
          contents += row
        }
        contents += new GridPanel(1, 2) {
          contents += new Label("Column:")
          contents += col
        }
        contents += new GridPanel(1, 2) {
          contents += new Label("Distance:")
          contents += distance
        }
        contents += new Button("Filter") {
          reactions += {
            case ButtonClicked(_) =>
              top.contents = controller.filterField(row.text.toInt, col.text.toInt, distance.text.toInt)
              close()
          }
        }
      }
      title = "Filter field"
      centerOnScreen()
      size = new Dimension(200, 200)
      open()
    }
  }

  private def showFractializeFieldDialog(): Unit = {
    new Dialog {
      contents = new GridPanel(3, 1) {
        val row = new TextField("0")
        val col = new TextField("0")

        contents += new GridPanel(1, 2) {
          contents += new Label("Row:")
          contents += row
        }
        contents += new GridPanel(1, 2) {
          contents += new Label("Column:")
          contents += col
        }
        contents += new Button("Fractialize") {
          reactions += {
            case ButtonClicked(_) =>
              top.contents = controller.fractializeField(row.text.toInt, col.text.toInt)
              close()
          }
        }
      }
      title = "Fractialize field"
      centerOnScreen()
      size = new Dimension(200, 200)
      open()
    }
  }

  private def playSequenceOfMoves(input: String): Unit = {
    if (controller.gameState != GameStates.PLAYING) return

    new Thread {
      override def run(): Unit = {
        IOParser.readCharPerLine(input).map(Converter.toDirection).foreach(direction => {
          Thread.sleep(500)
          top.contents = controller.move(direction)
        })
      }
    }.start()
  }
}