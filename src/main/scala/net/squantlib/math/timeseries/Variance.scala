package net.squantlib.math.timeseries

import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import net.squantlib.util.Date


object Variance {
	/**
	 * Returns variance
	 * @return variance of unsorted collection of data
	 */
	def calculate(values:Iterable[Double]):Double = {
		val logsetcount = values.size 
		val logmean = values.sum / logsetcount
		values.map(v => (v - logmean) * (v - logmean)).sum / logsetcount
	}
	
	/**
	 * Returns variance
	 * @return a variance for the full given period
	 */
	def calculate(quotes:SortedMap[Date, Double]):SortedMap[Date, Double] = calculate(quotes, quotes.size)
	
	/**
	 * Returns variance
	 * @param number of historical data per computed variance
	 * @return running variance over given period.
	 */
	def calculate(quotes:SortedMap[Date, Double], nbData:Int):SortedMap[Date, Double] = {
		val keys = quotes.keySet.toIndexedSeq; 
		TreeMap((nbData to quotes.size).map( i => { 
				val startdate = keys(i - nbData)
				val enddate = keys(i - 1)
				(enddate, calculate(quotes.from(startdate).to(enddate).map(v => v._2)))}) :_*)
	}  
}
