package squantlib.timeseries

import scala.collection.immutable.SortedMap
import squantlib.math.timeseries.{MovingAverage => MA}
import java.lang.{ Double => JDouble}
import org.jquantlib.time.TimeSeries


class MovingAverage(nbDays:Int) {
	val nbdays = if (nbDays <= 0) 0 else nbDays
	
	def calculate(quotes:TimeSeries[JDouble]) = {
	  val quotemap = STimeSeries.toSortedMap(quotes)
	  val result = {if (nbdays > 0) MA.calculate(quotemap, nbDays)
	  			   else MA.calculate(quotemap, quotemap.size)}
	  STimeSeries.toTimeSeries(result)
	}
	
	def this() = this(-1)
}