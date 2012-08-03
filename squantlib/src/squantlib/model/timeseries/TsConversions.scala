package squantlib.model.timeseries

import scala.collection.JavaConversions._
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import org.jquantlib.time.{TimeSeries, Date => qlDate, Period => qlPeriod }
import java.lang.{Double => JavaDouble}

/**
 * Conversion between TimeSeries and SortedMap
 * 
 * SortedMap[org.jquantlib.time.Date, scala.Double] is converted to TimeSeries[java.lang.Double]
 * either implicitly or through sortedmap.toTimeSeries method
 * 
 * TimeSeries[java.lang.Double] is converted to SortedMap[org.jquantlib.time.Date, scala.Double]
 * through timeseries.toSortedMap (no implicit implementation)
 * 
 */
object TsConversions {
  
    implicit def SortedMap2Ts(values:SortedMap[qlDate, Double]) : TimeSeries[JavaDouble] = 
      new TimeSeries[java.lang.Double](JavaDouble.TYPE, values.toMap.map(q => (q._1, new JavaDouble(q._2))))

	implicit def Ts2SortedMap(ts:TimeSeries[JavaDouble]) = new ConvertableTimeSeries(ts)
	implicit def Sortedmap2Ts(m:SortedMap[qlDate, Double]) = new ConvertableSortedMap(m)

	class ConvertableTimeSeries(ts:TimeSeries[JavaDouble]) {
	  def toSortedMap = SortedMap(ts.mapValues(d => d.doubleValue).toSeq:_*)
	}
	
	class ConvertableSortedMap(m:SortedMap[qlDate, Double]) {
	  def toTimeSeries = new TimeSeries[JavaDouble](JavaDouble.TYPE, m.map(q => (q._1, new JavaDouble(q._2))))
	}
}



