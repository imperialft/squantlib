package squantlib.timeseries

import scala.collection.immutable.SortedMap
import squantlib.math.timeseries.TSAnalysis
import java.lang.{ Double => JDouble}
import org.jquantlib.time.TimeSeries

class HistoricalCorrelation(nbDays:Int) {
	
	val nbdays = if (nbDays<=0) 0 else nbDays
  
	def calculate(quotes1:TimeSeries[JDouble], quotes2:TimeSeries[JDouble]) = {
	  val quotemap1 = STimeSeries.toSortedMap(quotes1)
	  val quotemap2 = STimeSeries.toSortedMap(quotes2)
	  
	  val result = nbdays match {
	  	case d if d > 0 => TSAnalysis.Correlation(quotemap1, quotemap2, nbdays)
	  	case _ => TSAnalysis.Correlation(quotemap1, quotemap2)
	  }
	  
	  STimeSeries.toTimeSeries(result)
	}
	
	def this() = this(-1)
}