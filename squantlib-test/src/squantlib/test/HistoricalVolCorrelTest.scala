package squantlib.test

import scala.collection.JavaConversions._

import squantlib.math.timeseries.TimeSeriesFunctions

import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.TimeSeries
import org.jquantlib.time.Date
import org.jquantlib.time.Month


object HistoricalVolCorrelTest {

    val dates = Array[(JDate, Double)] (
				(new JDate(26, Month.December, 2011), 77.98),
				(new JDate(27, Month.December, 2011), 77.88),
				(new JDate(28, Month.December, 2011), 77.94),
				(new JDate(29, Month.December, 2011), 77.64),
				(new JDate(30, Month.December, 2011), 76.91),
				(new JDate(2, Month.January, 2012), 76.90),
				(new JDate(3, Month.January, 2012), 76.74),
				(new JDate(4, Month.January, 2012), 76.72),
				(new JDate(5, Month.January, 2012), 77.12),
				(new JDate(6, Month.January, 2012), 76.97),
				(new JDate(9, Month.January, 2012), 76.86),
				(new JDate(10, Month.January, 2012), 76.85),
				(new JDate(11, Month.January, 2012), 76.85),
				(new JDate(12, Month.January, 2012), 76.76),
				(new JDate(13, Month.January, 2012), 76.97),
				(new JDate(16, Month.January, 2012), 76.78),
				(new JDate(17, Month.January, 2012), 76.83),
				(new JDate(18, Month.January, 2012), 76.82),
				(new JDate(19, Month.January, 2012), 77.11),
				(new JDate(20, Month.January, 2012), 77.01),
				(new JDate(23, Month.January, 2012), 77.02),
				(new JDate(24, Month.January, 2012), 77.67),
				(new JDate(25, Month.January, 2012), 77.78),
				(new JDate(26, Month.January, 2012), 77.45),
				(new JDate(27, Month.January, 2012), 76.70),
				(new JDate(30, Month.January, 2012), 76.35),
				(new JDate(31, Month.January, 2012), 76.27),
				(new JDate(1, Month.February, 2012), 76.20),
				(new JDate(2, Month.February, 2012), 76.22),
				(new JDate(3, Month.February, 2012), 76.60),
				(new JDate(6, Month.February, 2012), 76.55),
				(new JDate(7, Month.February, 2012), 76.76),
				(new JDate(8, Month.February, 2012), 77.04),
				(new JDate(9, Month.February, 2012), 77.67),
				(new JDate(10, Month.February, 2012), 77.61),
				(new JDate(13, Month.February, 2012), 77.57),
				(new JDate(14, Month.February, 2012), 78.44),
				(new JDate(15, Month.February, 2012), 78.43),
				(new JDate(16, Month.February, 2012), 78.94),
				(new JDate(17, Month.February, 2012), 79.55),
				(new JDate(20, Month.February, 2012), 79.63),
				(new JDate(21, Month.February, 2012), 79.74),
				(new JDate(22, Month.February, 2012), 80.29),
				(new JDate(23, Month.February, 2012), 80.00),
				(new JDate(24, Month.February, 2012), 81.20),
				(new JDate(27, Month.February, 2012), 80.61),
				(new JDate(28, Month.February, 2012), 80.46),
				(new JDate(29, Month.February, 2012), 81.15),
				(new JDate(1, Month.March, 2012), 81.12),
				(new JDate(2, Month.March, 2012), 81.47)
    )
    
    val testarray = Set[Double] (
	        -0.0012832030504524,
			0.000770119406564252,
			-0.00385654160975365,
			-0.00944685123184717,
			-0.000130030557364077,
			-0.00208279169279209,
			-0.000260654243623444,
			0.0052002197271075,
			-0.00194691475606423
	)

    val c:java.lang.Class[java.lang.Double] = java.lang.Double.TYPE

    val ts = new TimeSeries[java.lang.Double](c)
    for (i <- 0 to dates.length - 1) {
        ts.put(dates(i)._1, dates(i)._2);
    }	
  
	def main(args: Array[String]): Unit = {
        println("Variance")
        val testvar = TimeSeriesFunctions.Variance(testarray)
        println(testvar)
	  
        println("Log")
        // abs(ln(current/prev)) / sqrt(timefraction)
        val logvalues = TimeSeriesFunctions.LogReturn(ts)
        for (k <- logvalues.keySet) { println(k + " " + logvalues.get(k)) }
	  
	  
        println("TimeSeries Variance")
        // abs(ln(current/prev)) / sqrt(timefraction)
        val variancearray = TimeSeriesFunctions.Variance(ts, 10)
        for (k <- variancearray.keySet) { println(k + " " + variancearray.get(k)) }
        
        
	}

}