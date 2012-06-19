package squantlib.math.timeseries

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
object TSAnalysis {
	val defaultannualdays = 252
  
	/**
	 * Returns daily log return for given period.
	 * @return array of size n-1 containing ln(Xn / Xn-1)
	 */
	def LogReturn(values:SortedMap[JDate, Double]) : SortedMap[JDate, Double] = {
	  val keys = values.keySet.toIndexedSeq
	  TreeMap(((1 to (values.size-1)) map (i => (keys(i), math.log(values(keys(i)) / values(keys(i-1)))))) :_*)
	}
	
	/**
	 * Returns daily log return for given period.
	 * @return array of size n-1 containing ln(Xn / Xn-1)
	 */
	def LogReturn(values:Array[Double]) : Array[Double] = {
	  (1 to (values.size-1)) map (i => math.log(values(i) / values(i-1))) toArray
	}

	/**
	 * Returns variance
	 * @return variance of unsorted collection of data
	 */
	def Variance(values:Array[Double]):Double = {
		val logsetcount = values.size 
		val logmean = values.sum / logsetcount
		values.map(v => (v - logmean) * (v - logmean)).sum / logsetcount
	}
	
	/**
	 * Returns variance
	 * @return a variance for the full given period
	 */
	def Variance(quotes:SortedMap[JDate, Double]):SortedMap[JDate, Double] = Variance(quotes, quotes.size)
	
	/**
	 * Returns variance
	 * @param number of historical data per computed variance
	 * @return running variance over given period.
	 */
	def Variance(quotes:SortedMap[JDate, Double], nbData:Int):SortedMap[JDate, Double] = {
		val keys = quotes.keySet.toIndexedSeq; 
		TreeMap((nbData to quotes.size).map( i => { 
				val startdate = keys(i - nbData)
				val enddate = keys(i - 1)
				(enddate, Variance(quotes.from(startdate).to(enddate).map(v => v._2).toArray))}) :_*)
	}
	
	/**
	 * Returns Standard Deviation
	 * @return a variance for the full given period
	 */
    def StdDev(quotes:Array[Double]):Double = math.sqrt(Variance(quotes))
    
	/**
	 * Returns Standard Deviation
	 * @return a variance for the full given period
	 */
    def StdDev(quotes:SortedMap[JDate, Double]):SortedMap[JDate, Double] = StdDev(quotes, quotes.size)
    
	/**
	 * Returns standard deviation
	 * @param number of historical data per computed standard deviation
	 * @return running standard deviation over given period.
	 */
	def StdDev(quotes:SortedMap[JDate, Double], nbData:Int):SortedMap[JDate, Double] = Variance(quotes, nbData).map(v => (v._1, math.sqrt(v._2)))
	  
 	
	/**
	 * Returns an annualized volatility of log return assuming daily data & 252 days per year. No data validation.
	 */
    def HistoricalVol(quotes:SortedMap[JDate, Double]):SortedMap[JDate, Double] = HistoricalVol(quotes, quotes.size, defaultannualdays)
    
	/**
	 * Returns running annualized volatility of log return assuming daily data & 252 days per year, for the given period.
	 */
    def HistoricalVol(quotes:SortedMap[JDate, Double], nbData:Int):SortedMap[JDate, Double] = HistoricalVol(quotes, defaultannualdays)
    
	/**
	 * Returns running annualized volatility of log return assuming specified annual length, for the given period.
	 * @param number of data per year (ie. 252 if daily, 52 if weekly, etc)
	 */
    def HistoricalVol(quotes:SortedMap[JDate, Double], nbData:Int, annualDays:Double):SortedMap[JDate, Double] =
      StdDev(LogReturn(quotes), nbData - 1).map(s => (s._1, s._2 * math.sqrt(annualDays)))

	/**
	 * Returns covariance.
	 */
	def Covariance(quotes1:SortedMap[JDate, Double], quotes2:SortedMap[JDate, Double], nbData:Int):SortedMap[JDate, Double] = {
		require (quotes1.size == quotes2.size && quotes1.forall(q => quotes2.keySet contains q._1))
		val keys = quotes1.keySet.toIndexedSeq
		TreeMap((nbData to quotes1.size).map( i => { 
				val startdate = keys(i - nbData); val enddate = keys(i - 1);
				val q1 = quotes1.from(startdate).to(enddate).map(v => v._2).toArray
				val q2 = quotes2.from(startdate).to(enddate).map(v => v._2).toArray
				(enddate, Covariance(q1, q2))}) :_*)
	}

    def Covariance(quotes1:Array[Double], quotes2:Array[Double]) : Double = {
		require (quotes1.size == quotes2.size)
        val datacount = quotes1.size
        val sumlog1 = quotes1.sum
        val sumlog2 = quotes2.sum
        val summult = ((0 to datacount-1) map (i => quotes1(i) * quotes2(i))).sum
        summult / datacount - sumlog1 / datacount * sumlog2 / datacount
    }
    
	/**
	 * Returns historical correlation between two time series. Size of these time series must match.
	 */
    def Correlation(quotes1:Array[Double], quotes2:Array[Double]) : Double = {
		require (quotes1.size == quotes2.size)
		val logset1 = LogReturn(quotes1)
		val logset2 = LogReturn(quotes2)
        Covariance(logset1, logset2) / StdDev(quotes1) / StdDev(quotes2)
    }
    
	/**
	 * Returns historical correlation between two series of log returns over given days for the data period. Size and keys of these time series must match.
	 */
    def Correlation(quotes1:SortedMap[JDate, Double], quotes2:SortedMap[JDate, Double], nbData:Int) : SortedMap[JDate, Double] = {
		require (quotes1.size == quotes2.size && quotes1.forall(q => quotes2.keySet contains q._1))
		val logset1 = LogReturn(quotes1)
		val logset2 = LogReturn(quotes2)
		val keys = logset1.keySet.toIndexedSeq; 
        val datacount = nbData - 1
        
		TreeMap((datacount to logset1.size).map( i => { 
				val startdate = keys(i - datacount)
				val enddate = keys(i - 1)
				val q1 = logset1.from(startdate).to(enddate).map(v => v._2).toArray
				val q2 = logset2.from(startdate).to(enddate).map(v => v._2).toArray
				(enddate, Covariance(q1, q2) / StdDev(q1) / StdDev(q2))}) :_*)
    }
    
	/**
	 * Returns historical correlation between two series of log returns for the data period. Size and keys of these time series must match.
	 */
    def Correlation(quotes1:SortedMap[JDate, Double], quotes2:SortedMap[JDate, Double]) : SortedMap[JDate, Double] = Correlation(quotes1, quotes2, quotes1.size)
}