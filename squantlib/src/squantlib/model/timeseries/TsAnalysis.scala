package squantlib.model.timeseries

import scala.collection.SortedMap
import scala.collection.JavaConversions._
import squantlib.math.timeseries.{Correlation, Volatility, Variance, MovingAverage}
import java.lang.{ Double => JavaDouble}
import java.util.{Date => JavaDate}
import org.jquantlib.time.{TimeSeries, Date => qlDate, Period => qlPeriod }

object TsAnalysis {

    implicit def SortedMap2Ts(values:SortedMap[qlDate, Double]) : TimeSeries[JavaDouble] = 
      new TimeSeries[java.lang.Double](JavaDouble.TYPE, mapAsJavaMap(values.mapValues(q => q.doubleValue)))
        
    implicit def SortedMapJava2Ts(values:SortedMap[JavaDate, Double]) : TimeSeries[JavaDouble] = 
      new TimeSeries[java.lang.Double](JavaDouble.TYPE, values.map(q => (new qlDate(q._1), new JavaDouble(q._2))))

	implicit def Sortedmap2Ts(m:SortedMap[qlDate, Double]) = new ConvertableSortedMap(m)
	class ConvertableSortedMap(m:SortedMap[qlDate, Double]) {
	  def toTimeSeries = new TimeSeries[JavaDouble](JavaDouble.TYPE, m.map(q => (q._1, new JavaDouble(q._2))))
	}

	implicit def JavaSortedmap2Ts(m:SortedMap[JavaDate, Double]) = new ConvertableJavaSortedMap(m)
	class ConvertableJavaSortedMap(m:SortedMap[JavaDate, Double]) {
	  def toTimeSeries = new TimeSeries[JavaDouble](JavaDouble.TYPE, m.map(q => (new qlDate(q._1), new JavaDouble(q._2))))
	}
    
  
	implicit def TsToRichTs(ts:TimeSeries[JavaDouble]) = new RichTimeSeries(ts)
  
	class RichTimeSeries(ts:TimeSeries[JavaDouble]) {
	  
	  val tsmap = toSortedMap
	  def toSortedMap = SortedMap(ts.mapValues(d => d.doubleValue).toSeq:_*)
	  
	  def correlation(series:TimeSeries[JavaDouble], nbdays:Int = -1):TimeSeries[JavaDouble] = 
		Correlation.calculate(tsmap, series.toSortedMap, nbdays)
		
	  def volatility(nbdays:Int = -1, annualdays:Double = 260.0):TimeSeries[JavaDouble] = 
	    Volatility.calculate(tsmap, if (nbdays > 0) nbdays else tsmap.size, annualdays)
	  
	  def variance(nbDays:Int):TimeSeries[JavaDouble] =
		Variance.calculate(tsmap, nbDays)
	
	  def movingaverage(nbDays:Int):TimeSeries[JavaDouble] = 
		MovingAverage.calculate(tsmap, nbDays)
		
	  def describe = ts.foreach(t => println(t._1.shortDate + "\t" + t._2))
	}
	  
}