package squantlib.timeseries

import scala.collection.immutable.SortedMap
import squantlib.math.timeseries.Correlation
import java.lang.{ Double => JDouble}
import org.jquantlib.time.TimeSeries

class HistoricalCorrelation(val nbdays:Int = -1) {
  
	def calculate(quotes1:TimeSeries[JDouble], quotes2:TimeSeries[JDouble]) = {
	  val quotemap1 = STimeSeries.toSortedMap(quotes1)
	  val quotemap2 = STimeSeries.toSortedMap(quotes2)
	  
	  val result = nbdays match {
	  	case d if d > 0 => Correlation.calculate(quotemap1, quotemap2, nbdays)
	  	case _ => Correlation.calculate(quotemap1, quotemap2)
	  }
	  
	  STimeSeries.toTimeSeries(result)
	}
}