package squantlib.math.timeseries

import scala.collection.JavaConversions._
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import scala.collection

import org.jquantlib.time.TimeSeries
import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import java.lang.{ Double => JDouble }

object TimeSeriesFunctions {
  
	def LogReturn(values:SortedMap[JDate, Double]) : SortedMap[JDate, Double] = {
	  val keys = values.keySet.toIndexedSeq
	  TreeMap(((1 to (values.size-1)) map (i => (keys(i), math.log(values(keys(i)) / values(keys(i-1)))))) :_*)
	}

	def LogReturn(quotes:TimeSeries[JDouble]) : TimeSeries[JDouble] = {
		val dates = quotes.navigableKeySet.iterator
		var prev = quotes.get(dates.next)
		var retval = new TimeSeries[JDouble](JDouble.TYPE)
		
		while (dates.hasNext) {
		  val d = dates.next
		  val v = quotes.get(d)
		  retval.put(d, math.log(v / prev))
		  prev = v;
		}
	    retval
	}
	
	def Variance(values:Set[Double]):Double = {
		val logsetcount = values.size
		val logmean = values.sum / logsetcount
		values.map(v => (v - logmean) * (v - logmean)).sum / logsetcount
	}
	
	def Variance(quotes:TimeSeries[JDouble], nbData:Int):TimeSeries[JDouble] = {
		val quotesmap= TreeMap(quotes.map(q => (q._1, q._2.doubleValue)).toSeq :_*)
		val logset = LogReturn(quotesmap)
		val keys = logset.keySet.toIndexedSeq
		val calccount = nbData - 1
		val variance = (calccount to logset.size).map( i => {
			  val startdate = keys(i - calccount)
			  val enddate = keys(i - 1)
			  (enddate, new java.lang.Double(Variance(logset.range(startdate, enddate).map(v => v._2).toSet)))
			}) toMap
		var retval = new TimeSeries[JDouble](JDouble.TYPE)
		retval.putAll(variance)
		retval
		
	}

}