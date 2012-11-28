package squantlib.math.timeseries

import scala.collection.JavaConversions._
import squantlib.database.QLConstructors._
import java.lang.{ Double => JavaDouble}
import org.jquantlib.time.{TimeSeries, Date => qlDate, Period => qlPeriod }

object SeriesAnalysis {
  
	implicit def TimeSeriesToRichTs(ts:TimeSeries[JavaDouble]) = new RichTimeSeries(ts)
  
	class RichTimeSeries(ts:TimeSeries[JavaDouble]) {
	  
	  val tsmap = ts.toSortedMap
	  
	  def correlation(series:TimeSeries[JavaDouble], nbdays:Int = -1):TimeSeries[JavaDouble] = 
		Correlation.calculate(tsmap, series.toSortedMap, nbdays).toTimeSeries
		
	  def volatility(nbdays:Int = -1, annualdays:Double = 260.0):TimeSeries[JavaDouble] = 
	    Volatility.calculate(tsmap, if (nbdays > 0) nbdays else tsmap.size, annualdays).toTimeSeries
	  
	  def variance(nbDays:Int):TimeSeries[JavaDouble] =
		Variance.calculate(tsmap, nbDays).toTimeSeries
	
	  def movingaverage(nbDays:Int):TimeSeries[JavaDouble] = 
		MovingAverage.calculate(tsmap, nbDays).toTimeSeries
		
	  def describe = ts.foreach(t => println(t._1.shortDate.toString + "\t" + t._2))
	}
	  
}