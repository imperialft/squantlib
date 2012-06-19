package squantlib.timeseries

import scala.collection.immutable.SortedMap
import squantlib.math.timeseries.TSAnalysis
import java.lang.{ Double => JDouble}
import org.jquantlib.time.TimeSeries


class HistoricalVol(nbDays:Int, annualDays:Double) {
  
	val nbdays = if (nbDays <= 0) 0 else nbDays
	val annualdays = if (annualDays <= 0.0) 0.0 else annualDays
	
	def calculate(quotes:TimeSeries[JDouble]) = {
	  val quotemap = STimeSeries.toSortedMap(quotes)
	  val result = {if (nbdays > 0 && annualdays > 0.0) TSAnalysis.HistoricalVol(quotemap, nbDays, annualDays)
	  			   else if (nbdays > 0) TSAnalysis.HistoricalVol(quotemap, nbDays)
	  			   else TSAnalysis.HistoricalVol(quotemap)}
	  STimeSeries.toTimeSeries(result)
	}
	
	def this(nbDays:Int) = this(nbDays, -1.0)
	def this() = this(-1, -1.0)
	
}

class HistoricalVariance(val nbDays:Int) {
  
	def calculate(quotes:TimeSeries[JDouble]) = 
	  STimeSeries.toTimeSeries(TSAnalysis.Variance(STimeSeries.toSortedMap(quotes), nbDays))
}


