package squantlib.model.timeseries

import scala.collection.immutable.SortedMap
import squantlib.math.timeseries.{Correlation, Volatility, Variance, MovingAverage}
import java.lang.{ Double => JavaDouble}
import org.jquantlib.time.TimeSeries

import scala.collection.JavaConversions._
import scala.collection.immutable.TreeMap
import org.jquantlib.time.{TimeSeries, Date => JDate, Period => JPeriod }
import squantlib.model.timeseries.TsConversions._

object TsAnalysis {
  
	implicit def TsToRichTs(ts:TimeSeries[JavaDouble]) = new RichTimeSeries(ts)
  
	class RichTimeSeries(ts:TimeSeries[JavaDouble]) {
	  val tsmap = ts.toSortedMap
	  
	  def correlation(series:TimeSeries[JavaDouble], nbdays:Int = -1):TimeSeries[JavaDouble] = 
		Correlation.calculate(tsmap, series.toSortedMap, nbdays)
		
	  def volatility(nbdays:Int = -1, annualdays:Double = 260.0):TimeSeries[JavaDouble] = 
	    Volatility.calculate(tsmap, if (nbdays > 0) nbdays else tsmap.size, annualdays)
	  
	  def variance(nbDays:Int):TimeSeries[JavaDouble] =
		Variance.calculate(tsmap, nbDays)
	
	  def movingaverage(nbDays:Int):TimeSeries[JavaDouble] = 
		MovingAverage.calculate(tsmap, nbDays)
	}
	  
}