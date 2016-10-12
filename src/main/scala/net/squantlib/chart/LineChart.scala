package net.squantlib.chart

import java.io.File
import java.lang.Number
import scala.collection.mutable.ListBuffer
import org.jfree.chart._
import org.jfree.chart.plot._
import org.jfree.data.xy._


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