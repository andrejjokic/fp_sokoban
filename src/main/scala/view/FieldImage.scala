package sokoban
package view

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object FieldImage {
  private val emptySrc = "src/main/resources/empty.jpg"
  private val boxSrc = "src/main/resources/box.jpg"
  private val wallSrc = "src/main/resources/wall.jpg"
  private val goalSrc = "src/main/resources/goal.jpg"
  private val playerSrc = "src/main/resources/player.jpg"
  private val boxOnGoalSrc = "src/main/resources/box_on_goal.jpg"

  val empty: BufferedImage = ImageIO.read(new File(emptySrc))
  val box: BufferedImage = ImageIO.read(new File(boxSrc))
  val wall: BufferedImage = ImageIO.read(new File(wallSrc))
  val goal: BufferedImage = ImageIO.read(new File(goalSrc))
  val player: BufferedImage = ImageIO.read(new File(playerSrc))
  val boxOnGoal: BufferedImage = ImageIO.read(new File(boxOnGoalSrc))
}
