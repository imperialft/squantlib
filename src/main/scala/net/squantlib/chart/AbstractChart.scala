package net.squantlib.chart

import java.io.File
import java.lang.Number
import org.jfree.chart._

abstract class AbstractChart {
  
  protected def save(png:String, chart:JFreeChart, width:Integer = 640, height:Integer = 480):String = {
    ChartUtilities.saveChartAsPNG(new File(png), chart, width, height)
    return png
  }

  def render(png:String, data:Seq[Pair[Number, Number]], x:String = "X-axis", y:String = "Y-axis", title:String = "Untitled"):String

  /**
   * Renders a line chart onto a Window. The image rendered will be saved onto a temporary file. Probably blocks the program until the window closes.
   *
   * @param data Data to plot. In Seq[Pair[X, Y]] format.
   * @param x Label for X-axis
   * @param y Label for Y-axis
   * @param width Image and window width
   * @param width Image and window height
   * @param title Title for the plot.
   *
   */
  def display(data:Seq[Pair[Number, Number]], x:String = "X-axis", y:String = "Y-axis", title:String = "Untitled", width:Integer = 640, height:Integer = 480):Unit = {
    val png = render(File.createTempFile("squantlib-", ".png").getPath(), x = x, y = y, title = title, data = data)
    new Window(imagePath = png, width = width, height = height, title = title)
  }
}

