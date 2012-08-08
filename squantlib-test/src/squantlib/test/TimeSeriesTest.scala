package squantlib.test

import scala.collection.JavaConversions._
import scala.collection.immutable.TreeMap
import squantlib.test.sample.TimeSeriesSamples
import squantlib.math.timeseries._
import squantlib.model.timeseries.TsAnalysis._
import org.jquantlib.QL
import org.jquantlib.model.volatility._
import org.jquantlib.time.TimeSeries
import org.junit.Test
import scala.collection.SortedMap
//import squantlib.model.timeseries.TsConversions.Ts2SortedMap

object TimeSeriesTest {
  
    val data = TimeSeriesSamples.USDFX
    val data2 = TimeSeriesSamples.EURFX
    
    val datamap = data.map(d => (d._1, new java.lang.Double(d._2))).toMap
    val ts = new TimeSeries[java.lang.Double](java.lang.Double.TYPE, datamap)
    val datamap2 = data2.map(d => (d._1 -> new java.lang.Double(d._2))).toMap
    val ts2 = new TimeSeries[java.lang.Double](java.lang.Double.TYPE, datamap2)

	def main(args: Array[String]): Unit = {
	    println("*** TS ***")
	    for (k <- ts.keySet) { println(k + " " + ts.get(k)) }
	    
	    println("*** SimpleLocalEstimator ***")
	    // abs(ln(current/prev)) / sqrt(timefraction)
	    val sle = new SimpleLocalEstimator(1/360.0);
	    val locale = sle.calculate(ts); 
	    for (k <- locale.keySet) { println(k + " " + locale.get(k)) }
	    
	    println("*** ConstantEstimator ***")
	    // sqrt(sum(x^2)/size - sum(x)*sum(x) / size*(size-1))
	    val constmodel = new ConstantEstimator(4);
	    val const = constmodel.calculate(ts);
	    for (k <- const.keySet) { println(k + " " + const.get(k)) }
	    
	    println("*** GARCH model ***")
//	    val garchmodel = new Garch11(0.2, 0.1, 0.1);
	    val garchmodel = new Garch11(0.9, 0.08, 0.02);
	    val garch = garchmodel.calculate(ts);
	    for (k <- garch.keySet) { println(k + " " + garch.get(k)) }

		val fxsorted = TreeMap(data :_*)
		val fxsorted2 = TreeMap(data2 :_*) 
		val annualdays = 260
		
        println("*** Log ***")
        val logvalues = LogReturn.calculate(fxsorted)
        for (k <- logvalues.keySet) { println(k + " " + logvalues(k)) }
	  
        println("*** Array Variance ***")
        val variancearray = Variance.calculate(LogReturn.calculate(fxsorted), 10)
        for (k <- variancearray.keySet) { println(k + " " + variancearray(k)) }
        
        println("*** Array StdDev ***")
        val stddevarray = StdDev.calculate(LogReturn.calculate(fxsorted), 10)
        for (k <- stddevarray.keySet) { println(k + " " + stddevarray(k)) }
        
        println("*** Array Volatility ***")
        val volarray = Volatility.calculate(fxsorted, 10, 252.0)
        for (k <- volarray.keySet) { println(k + " " + volarray(k)) }
        
        println("*** Array Covariance ***")
        val covararray = Covariance.calculate(fxsorted, fxsorted2, 10)
        for (k <- covararray.keySet) { println(k + " " + covararray(k)) }
        
        println("*** Array Correlation ***")
        val correlarray = Correlation.calculate(fxsorted, fxsorted2, 10)
        for (k <- correlarray.keySet) { println(k + " " + correlarray(k)) }
        
        println("*** TimeSeries Volatility ***")
        val testvol = ts.volatility(10, 260.0)
        for (k <- testvol.keySet) { println(k + " " + testvol.get(k)) }
        
        println("*** TimeSeries Correlation ***")
        val correls = ts.correlation(ts2, 10)
        for (k <- correls.keySet) { println(k + " " + correls.get(k)) }
        
        println("*** Array MovingAverage ***")
        val movavearray = ts.movingaverage(10)
        for (k <- movavearray.keySet) { println(k + " " + movavearray(k)) }
	    
	}

}

