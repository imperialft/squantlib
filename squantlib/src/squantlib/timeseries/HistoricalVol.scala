package squantlib.timeseries

import scala.collection.immutable.SortedMap
import squantlib.math.timeseries.Volatility
import squantlib.math.timeseries.Variance
import java.lang.{ Double => JDouble}
import org.jquantlib.time.TimeSeries


class HistoricalVol(nbDays:Int, annualDays:Double) {
  
	val nbdays = if (nbDays <= 0) 0 else nbDays
	val annualdays = if (annualDays <= 0.0) 0.0 else annualDays
	
	def calculate(quotes:TimeSeries[JDouble]) = {
	  val quotemap = STimeSeries.toSortedMap(quotes)
	  val result = {if (nbdays > 0 && annualdays > 0.0) Volatility.calculate(quotemap, nbDays, annualDays)
	  			   else if (nbdays > 0) Volatility.calculate(quotemap, nbDays)
	  			   else Volatility.calculate(quotemap)}
	  STimeSeries.toTimeSeries(result)
	}
	
	def this(nbDays:Int) = this(nbDays, -1.0)
	def this() = this(-1, -1.0)
	
}

class HistoricalVariance(val nbDays:Int) {
  
	def calculate(quotes:TimeSeries[JDouble]) = 
	  STimeSeries.toTimeSeries(Variance.calculate(STimeSeries.toSortedMap(quotes), nbDays))
}


