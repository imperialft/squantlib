package squantlib.math.timeseries

import scala.collection.SortedMap
import org.jquantlib.time.{ Date => qlDate }

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
    def calculate(quotes:SortedMap[qlDate, Double]):SortedMap[qlDate, Double] = calculate(quotes, quotes.size)
    
	/**
	 * Returns standard deviation
	 * @param number of historical data per computed standard deviation
	 * @return running standard deviation over given period.
	 */
	def calculate(quotes:SortedMap[qlDate, Double], nbData:Int):SortedMap[qlDate, Double] = Variance.calculate(quotes, nbData).map(v => (v._1, math.sqrt(v._2)))
	
}