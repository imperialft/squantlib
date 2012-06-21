package squantlib.model.timeseries

import scala.collection.immutable.SortedMap
import squantlib.math.timeseries.Volatility
import squantlib.math.timeseries.Variance
import java.lang.{ Double => JDouble}
import org.jquantlib.time.TimeSeries


class HistoricalVol(val nbdays:Int = -1, val annualdays:Double = 252) {
	
	def calculate(quotes:TimeSeries[JDouble]) = {
	  val quotemap = STimeSeries.toSortedMap(quotes)
	  val result = {if (nbdays > 0 && annualdays > 0.0) Volatility.calculate(quotemap, nbdays, annualdays)
	  			   else if (nbdays > 0) Volatility.calculate(quotemap, nbdays)
	  			   else Volatility.calculate(quotemap)}
	  STimeSeries.toTimeSeries(result)
	}
	
}

class HistoricalVariance(val nbDays:Int) {
  
	def calculate(quotes:TimeSeries[JDouble]) = 
	  STimeSeries.toTimeSeries(Variance.calculate(STimeSeries.toSortedMap(quotes), nbDays))
}


