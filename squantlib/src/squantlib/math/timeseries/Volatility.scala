package squantlib.math.timeseries

import scala.collection.JavaConversions._
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap

import org.jquantlib.time.{TimeSeries, Date => qlDate, Period => qlPeriod }
import java.lang.{ Double => JDouble }

object Volatility {
	val defaultannualdays = 260
	
	/**
	 * Returns an annualized volatility of log return.
	 */
	def calculate(quotes:Array[Double], annualdays:Int):Double = StdDev.calculate(LogReturn.calculate(quotes).toSet) * math.sqrt(annualdays)
	
	/**
	 * Returns an annualized volatility of log return assuming daily data & 252 days per year. No data validation.
	 */
    def calculate(quotes:SortedMap[qlDate, Double]):SortedMap[qlDate, Double] = calculate(quotes, quotes.size, defaultannualdays)
    
	/**
	 * Returns running annualized volatility of log return assuming daily data & 252 days per year, for the given period.
	 */
    def calculate(quotes:SortedMap[qlDate, Double], nbData:Int):SortedMap[qlDate, Double] = calculate(quotes, defaultannualdays)
    
	/**
	 * Returns running annualized volatility of log return assuming specified annual length, for the given period.
	 * @param number of data per year (ie. 252 if daily, 52 if weekly, etc)
	 */
    def calculate(quotes:SortedMap[qlDate, Double], nbData:Int, annualDays:Double):SortedMap[qlDate, Double] =
      StdDev.calculate(LogReturn.calculate(quotes), nbData - 1).map(s => (s._1, s._2 * math.sqrt(annualDays)))
      
 	
}

