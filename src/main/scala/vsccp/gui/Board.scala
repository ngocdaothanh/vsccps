package vsccp.gui

import scala.swing.Component
import java.awt._

class Board extends Component {
  override def paint(g: Graphics2D) {
    g.drawString("hi", 0, 0)
  }
}
