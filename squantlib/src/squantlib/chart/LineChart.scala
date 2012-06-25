package squantlib.chart

import java.io.File
import java.lang.Number
import scala.collection.mutable.ListBuffer
import org.jfree.chart._
import org.jfree.chart.plot._
import org.jfree.data.xy._

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

object LineChart extends AbstractChart {
  /**
   * Renders a line chart to a PNG file.
   *
   * @param png Path for output file.
   * @param data Data to plot. In Seq[Pair[X, Y]] format.
   * @param x Label for X-axis
   * @param y Label for Y-axis
   * @param title Title for the plot.
   * @return Path to the output file (= png)
   *  
   */
  def render(png:String, data:Seq[Pair[Number, Number]], x:String = "X-axis", y:String = "Y-axis", title:String = "Untitled"):String = {
    val series = new XYSeries(title)
    for (pair <- data) series.add(pair._1, pair._2)
    val dataset = new XYSeriesCollection
    dataset.addSeries(series)
    val chart = ChartFactory.createXYLineChart(
                  title,
                  x, y,
                  dataset,
                  PlotOrientation.VERTICAL,
                  false,
                  false,
                  false
                )
    return save(png, chart)
  }

  def render(png:String, data_x:Seq[Number], data_y:Seq[Number]):String = {
    val buf = new ListBuffer[Pair[Number,Number]]()
    for (x <- data_x; y <- data_y)
      buf += Pair(x, y)
    return render(png, buf)
  }

  def render(png:String, it:Iterable[Pair[Number,Number]]):String = {
    val buf = new ListBuffer[Pair[Number,Number]]()
    it.foreach((item:Pair[Number,Number]) => buf += item)
    return render(png, buf.toSeq)
  }
}