package sokoban
package view

import java.awt.image.BufferedImage
import scala.swing.{Dimension, Graphics2D, Panel}

class Field(val image: BufferedImage) extends Panel {
  preferredSize = new Dimension(55, 55)

  override def paint(g: Graphics2D): Unit = {
    g.drawImage(image, 0, 0, size.width, size.height, null)
  }
}