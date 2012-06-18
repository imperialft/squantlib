package squantlib.test

import scala.collection.JavaConversions._

import org.jquantlib.QL
import org.jquantlib.model.volatility._
import org.jquantlib.time.Date
import org.jquantlib.time.Month
import org.jquantlib.time.TimeSeries
import org.junit.Test

object TimeSeriesTest {
 
    val dates = Array[Date] (
            new Date(25, Month.March, 2005),
            new Date(29, Month.March, 2005),
            new Date(15, Month.March, 2005),
            new Date(21, Month.March, 2005),
            new Date(27, Month.March, 2005))

    val values = Array[Double](1.2, 2.3, 0.3, 2.0, 2.5)
    
    val c:java.lang.Class[java.lang.Double] = java.lang.Double.TYPE

    val ts = new TimeSeries[java.lang.Double](c)
    for (i <- 0 to dates.length - 1) {
        ts.put(dates(i), values(i));
    }	
 
    @Test
    def testSECalculate():Unit = {
        val sle = new SimpleLocalEstimator(1/360.0);
        val locale = sle.calculate(ts);
        
//        assertNotNull(locale) ;
    }

    @Test
    def testCECalculate():Unit = {
        val sle = new SimpleLocalEstimator(1/360.0);
        val locale = sle.calculate(ts);
        val ce = new ConstantEstimator(1);
        val value = ce.calculate(locale);
//        assertNotNull(value) ;
    }

	  def main(args: Array[String]): Unit = {
        println("TS")
        for (k <- ts.keySet) { println(k + " " + ts.get(k)) }
        
        println("SimpleLocalEstimator")
        // abs(ln(current/prev)) / sqrt(timefraction)
        val sle = new SimpleLocalEstimator(1/360.0);
        val locale = sle.calculate(ts);
        for (k <- locale.keySet) { println(k + " " + locale.get(k)) }
        
        println("ConstantEstimator")
        // sqrt(sum(x^2)/size - sum(x)*sum(x) / size*(size-1))
        val constmodel = new ConstantEstimator(4);
        val const = constmodel.calculate(ts);
        for (k <- const.keySet) { println(k + " " + const.get(k)) }
        
        println("GARCH model")
        val garchmodel = new Garch11(0.2, 0.1, 0.1);
        val garch = garchmodel.calculate(ts);
        for (k <- garch.keySet) { println(k + " " + garch.get(k)) }
        
	  }

}