package squantlib.task.pricing

import squantlib.database._
import java.lang.{Double => JavaDouble}
import org.jquantlib.time.TimeSeries
import squantlib.database.schemadefinitions.Volatility
//import squantlib.model.timeseries.TsAnalysis._
import scala.collection.JavaConversions._ 
import org.jquantlib.time.{ Date => qlDate }
import scala.collection.mutable.{Queue, SynchronizedQueue, HashMap, SynchronizedMap, MutableList}
import squantlib.math.timeseries.{Volatility => VolScala}
import scala.collection.SortedMap

object Volatilities {
  
  var storedprice = new SynchronizedQueue[Volatility]
  private var storedts = new HashMap[String, TimeSeries[JavaDouble]] with SynchronizedMap[String, TimeSeries[JavaDouble]]
  
  def push:Unit =  {
    if (storedprice.size != 0) {
	    printf("Writing " + storedprice.size + " items to Database...")
		val t1 = System.nanoTime
		DB.insertOrUpdate(storedprice, false)
		val t2 = System.nanoTime
		printf("done (%.3f sec)\n".format(((t2 - t1)/1000000000.0)))
		storedprice.clear
		}
	}

  def clear:Unit = storedprice.clear
  
  def price(underlying:String, source:() => TimeSeries[JavaDouble], nbDays:Int = -1, startDate:qlDate, endDate:qlDate, annualDays:Int = 260):Unit = {
    
    if (storedts.isEmpty || !storedts.keySet.contains(underlying)) storedts(underlying) = source()
      
    val series = storedts(underlying) 
    
    var outputstring = ""
    def output(s:String):Unit = { outputstring += s }
    def outputln(s:String):Unit = { outputstring += s + "\n"}
	outputln("\n*** START OUTPUT VOL CALCULATION " + underlying + " (" + nbDays + "days) ***")
	
    if (series.isEmpty) {
      outputln("Error - Empty Set")
      printf(outputstring)
      return
    }
	
	val ts = series.map(s => (s._1, s._2.doubleValue))
	
    if (ts.size < nbDays) {
      outputln("Error - Not enough elements: found " + ts.size + " require " + nbDays)
      printf(outputstring)
      return
    }
	
	val sortedts = SortedMap(ts.toSeq:_*)
	val volresult = VolScala.calculate(sortedts, nbDays, annualDays)
	val resultseries = volresult.filter(c => ((c._1 ge startDate) && (c._1 le endDate) && (!c._2.isNaN)))
	val currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
	
	val result = resultseries.map { v =>
    	new Volatility(
	      id = (underlying + ":" + ("%tY%<tm%<td" format v._1.longDate) + ":" + 1 + ":" + nbDays),
	      underlying = underlying,
	      valuedate = v._1.longDate,
	      periodicity = 1,
	      nbdays = nbDays,
	      value = v._2,
	      lastmodified = Some(currenttime))
    	}
	
	outputln("source:\t" + series.size + " data from " + series.firstKey.shortDate + " to " + series.lastKey.shortDate)
	
	if (resultseries.keySet.isEmpty) outputln("result: empty")
	else outputln("result:\t" + resultseries.size + " data from " + resultseries.keySet.min.shortDate + " to " + resultseries.keySet.max.shortDate)
		
	outputln("errors:\t" + resultseries.filter(_._2.isNaN).size)
	storedprice ++= result
	outputln("total price:\t" + storedprice.size)
	printf(outputstring)
  }
    
}

