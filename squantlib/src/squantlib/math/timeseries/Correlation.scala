package squantlib.math.timeseries

import scala.collection.SortedMap
import org.jquantlib.time.{ Date => qlDate }

object Correlation {
  
	/**
	 * Returns historical correlation between two time series. Size of these time series must match.
	 */
    def calculate(quotes1:Array[Double], quotes2:Array[Double]) : Double = {
		require (quotes1.size == quotes2.size)
		val logset1 = LogReturn.calculate(quotes1)
		val logset2 = LogReturn.calculate(quotes2)
		if (logset1.forall(_ - 0.0 < 0.00000001) || logset2.forall(_ - 0.0 < 0.00000001)) 0.0 
		else Covariance.calculate(logset1, logset2) / StdDev.calculate(quotes1.toSet) / StdDev.calculate(quotes2.toSet)
    }
    
	/**
	 * Returns historical correlation between two series of log returns over given days for the data period. Size and keys of these time series must match.
	 */
    def calculate(quotes1:SortedMap[qlDate, Double], quotes2:SortedMap[qlDate, Double], nbData:Int) : SortedMap[qlDate, Double] = {
		require (quotes1.size == quotes2.size && quotes1.forall(q => quotes2.keySet contains q._1))
		val logset1 = LogReturn.calculate(quotes1)
		val logset2 = LogReturn.calculate(quotes2)
		val keys = logset1.keySet.toIndexedSeq; 
        val datacount = nbData - 1
        
		SortedMap((datacount to logset1.size).map( i => { 
				val startdate = keys(i - datacount)
				val enddate = keys(i - 1)
				val q1 = logset1.from(startdate).to(enddate).map(v => v._2).toArray
				val q2 = logset2.from(startdate).to(enddate).map(v => v._2).toArray
				val v = if (q1.forall(_ - 0.0 < 0.00000001) || q2.forall(_ - 0.0 < 0.00000001)) 0.0 
					else Covariance.calculate(q1, q2) / StdDev.calculate(q1.toSet) / StdDev.calculate(q2.toSet)
				(enddate, v)}) :_*)
    }
    
	/**
	 * Returns historical correlation between two series of log returns for the data period. Size and keys of these time series must match.
	 */
    def calculate(quotes1:SortedMap[qlDate, Double], quotes2:SortedMap[qlDate, Double]) : SortedMap[qlDate, Double] = calculate(quotes1, quotes2, quotes1.size)
}

