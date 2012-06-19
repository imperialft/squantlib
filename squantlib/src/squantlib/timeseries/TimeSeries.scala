package squantlib.timeseries

import scala.collection.JavaConversions._
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap

import org.jquantlib.time.TimeSeries
import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import java.lang.{ Double => JDouble }

/**
 * Analysis functions on time series data.
 */
object STimeSeries {
  
    def toSortedMap(quotes:TimeSeries[JDouble]) = TreeMap(quotes.map(q => (q._1, q._2.doubleValue)).toSeq :_*)
    def toMap(quotes:TimeSeries[JDouble]) : Map[JDate, Double] = quotes.map(q => (q._1, q._2.doubleValue)).toMap
    
    def toTimeSeries(quotes:Map[JDate, Double]) = new TimeSeries[JDouble](JDouble.TYPE, quotes.map(q => (q._1, new java.lang.Double(q._2))))
    def toTimeSeries(quotes:SortedMap[JDate, Double]) : TimeSeries[JDouble] = toTimeSeries(quotes.toMap)
    
    def apply(quotes:TimeSeries[JDouble], f:(SortedMap[JDate, Double] => SortedMap[JDate, Double])) = toTimeSeries(f(toSortedMap(quotes)))
    
}


