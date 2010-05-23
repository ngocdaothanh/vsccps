package vsccp.gui

import scala.swing._
import scala.swing.event._

import vsccp.logic._

object Main extends SimpleSwingApplication {
  val b = new vsccp.logic.Board

  def top = new MainFrame {
    title = "Very Simple Chinese Chess Program"
    
    resizable = false
    size = new java.awt.Dimension(400, 400)

    val button = new Button { text = "Click me" }

    contents = button
  }
}
