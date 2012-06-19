package squantlib.math.timeseries

import scala.collection.SortedMap
import org.jquantlib.time.{ Date => JDate }

object StdDev{
	
	/**
	 * Returns Standard Deviation
	 * @return a variance for the full given period
	 */
    def calculate(quotes:Set[Double]):Double = math.sqrt(Variance.calculate(quotes))
    
	/**
	 * Returns Standard Deviation
	 * @return a variance for the full given period
	 */
    def calculate(quotes:SortedMap[JDate, Double]):SortedMap[JDate, Double] = calculate(quotes, quotes.size)
    
	/**
	 * Returns standard deviation
	 * @param number of historical data per computed standard deviation
	 * @return running standard deviation over given period.
	 */
	def calculate(quotes:SortedMap[JDate, Double], nbData:Int):SortedMap[JDate, Double] = Variance.calculate(quotes, nbData).map(v => (v._1, math.sqrt(v._2)))
	
}