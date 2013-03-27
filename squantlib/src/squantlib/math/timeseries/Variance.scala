package squantlib.math.timeseries

import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import org.jquantlib.time.{ Date => JDate }


object Variance {
	/**
	 * Returns variance
	 * @return variance of unsorted collection of data
	 */
	def calculate(values:Set[Double]):Double = {
		val logsetcount = values.size 
		val logmean = values.sum / logsetcount
		values.map(v => (v - logmean) * (v - logmean)).sum / logsetcount
	}
	
	/**
	 * Returns variance
	 * @return a variance for the full given period
	 */
	def calculate(quotes:SortedMap[JDate, Double]):SortedMap[JDate, Double] = calculate(quotes, quotes.size)
	
	/**
	 * Returns variance
	 * @param number of historical data per computed variance
	 * @return running variance over given period.
	 */
	def calculate(quotes:SortedMap[JDate, Double], nbData:Int):SortedMap[JDate, Double] = {
		val keys = quotes.keySet.toIndexedSeq; 
		TreeMap((nbData to quotes.size).map( i => { 
				val startdate = keys(i - nbData)
				val enddate = keys(i - 1)
				(enddate, calculate(quotes.from(startdate).to(enddate).map(v => v._2).toSet))}) :_*)
	}  
}
