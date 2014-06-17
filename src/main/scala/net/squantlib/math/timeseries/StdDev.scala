package net.squantlib.math.timeseries

import scala.collection.SortedMap
import net.squantlib.util.Date

object StdDev{
	
	/**
	 * Returns Standard Deviation
	 * @return a variance for the full given period
	 */
    def calculate(quotes:Iterable[Double]):Double = math.sqrt(Variance.calculate(quotes))
    
	/**
	 * Returns Standard Deviation
	 * @return a variance for the full given period
	 */
    def calculate(quotes:SortedMap[Date, Double]):SortedMap[Date, Double] = calculate(quotes, quotes.size)
    
	/**
	 * Returns standard deviation
	 * @param number of historical data per computed standard deviation
	 * @return running standard deviation over given period.
	 */
	def calculate(quotes:SortedMap[Date, Double], nbData:Int):SortedMap[Date, Double] = Variance.calculate(quotes, nbData).map(v => (v._1, math.sqrt(v._2)))
	
}