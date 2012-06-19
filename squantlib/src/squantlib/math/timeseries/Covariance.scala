package squantlib.math.timeseries

import scala.collection.immutable.SortedMap
import scala.collection.immutable.TreeMap
import org.jquantlib.time.{Date => JDate}

object Covariance{
  	/**
	 * Returns historical covariance between two time series. Size of these time series must match.
	 */
    def calculate(quotes1:Array[Double], quotes2:Array[Double]) : Double = {
		require (quotes1.size == quotes2.size)
        val datacount = quotes1.size
        val sumlog1 = quotes1.sum
        val sumlog2 = quotes2.sum
        val summult = ((0 to datacount-1) map (i => quotes1(i) * quotes2(i))).sum
        summult / datacount - sumlog1 / datacount * sumlog2 / datacount
    }
  
  	/**
	 * Returns historical covariance between two time series. Size and keys of these time series must match.
	 */
	def calculate(quotes1:SortedMap[JDate, Double], quotes2:SortedMap[JDate, Double], nbData:Int):SortedMap[JDate, Double] = {
		require (quotes1.size == quotes2.size && quotes1.forall(q => quotes2.keySet contains q._1))
		val keys = quotes1.keySet.toIndexedSeq
		TreeMap((nbData to quotes1.size).map( i => { 
				val startdate = keys(i - nbData); val enddate = keys(i - 1);
				val q1 = quotes1.from(startdate).to(enddate).map(v => v._2).toArray
				val q2 = quotes2.from(startdate).to(enddate).map(v => v._2).toArray
				(enddate, calculate(q1, q2))}) :_*)
	}

}