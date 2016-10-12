package net.squantlib.chart

import java.awt._
import java.awt.event._

class Window(imagePath:String, width:Integer = 640, height:Integer = 480, title:String = "Untitled") extends Frame(title) {
  
  setSize(width, height)
  setVisible(true)
  setResizable(false)
  
  addWindowListener(
    new WindowAdapter {
      override def windowClosing(e:WindowEvent):Unit = System.exit(0)
    }
  )
  
  override def update(g:Graphics):Unit = paint(g)
  
  override def paint(g:Graphics):Unit = {
    g.drawImage(getToolkit().getImage(imagePath), 0, 0, this)
  }
  
}
