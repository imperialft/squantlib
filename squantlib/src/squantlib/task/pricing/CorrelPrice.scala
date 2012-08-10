package squantlib.task.pricing

import squantlib.database._
import java.lang.{Double => JavaDouble}
import org.jquantlib.time.TimeSeries
import squantlib.database.schemadefinitions.Correlation
import squantlib.model.timeseries.TsAnalysis._
import scala.collection.JavaConversions._ 
import org.jquantlib.time.{ Date => qlDate }
import scala.collection.mutable.{HashSet, SynchronizedSet, HashMap, SynchronizedMap}
import squantlib.math.timeseries.Correlation
import scala.collection.SortedMap

object CorrelPrice {
  
  private var pendingprice = new HashSet[Correlation] with SynchronizedSet[Correlation]
  private var storedts = new HashMap[String, TimeSeries[JavaDouble]] with SynchronizedMap[String, TimeSeries[JavaDouble]]
  
  def storedprice = pendingprice
   
  def push:Unit = {
    if (pendingprice.size != 0) {
		val targetprices = pendingprice.filter(!_.value.isNaN)
	    printf("Writing " + targetprices.size + " items to Database...")
		val t1 = System.nanoTime
		DB.insertOrReplace(targetprices)
		val t2 = System.nanoTime
		printf("done (%.3f sec)\n".format(((t2 - t1)/1000000000.0)))
		pendingprice.clear
		}
	}
  
  def clear:Unit = pendingprice.clear
  
  def price(underlying1:String, source1:() => TimeSeries[JavaDouble], underlying2:String, source2:() => TimeSeries[JavaDouble], nbDays:Int = -1, startDate:qlDate, endDate:qlDate):Unit = {
    
    if (storedts.isEmpty || !storedts.keySet.contains(underlying1)) storedts(underlying1) = source1()
    if (storedts.isEmpty || !storedts.keySet.contains(underlying2)) storedts(underlying2) = source2()
      
    val series1 = storedts(underlying1) 
    val series2 = storedts(underlying2) 
    
    var outputstring = ""
    def output(s:String):Unit = { outputstring += s }
    def outputln(s:String):Unit = { outputstring += s + "\n"}
	outputln("\n*** START OUTPUT CORREL CALCULATION " + underlying1 + ":" + underlying2 + " (" + nbDays + "days) ***")
	
    if (series1.isEmpty || series2.isEmpty) {
      outputln("Error - Empty Set")
      printf(outputstring)
      return
    }
	
	/**
	 * Creates factory from given paramset.
	 */
	val ts1 = SortedMap(series1.mapValues(d => d.doubleValue).toSeq:_*)
	val ts2 = SortedMap(series2.mapValues(d => d.doubleValue).toSeq:_*)
	val resultseries = Correlation.calculate(ts1, ts2, nbDays).filter(c => ((c._1 ge startDate) && (c._1 le endDate)))
	
	val result = resultseries.map { v =>
    	new Correlation(
	      id = (underlying1 + ":" + underlying2 + ":" + ("%tY%<tm%<td" format v._1.longDate) + ":" + 1 + ":" + nbDays),
	      underlying1 = underlying1,
	      underlying2 = underlying2,
	      valuedate = v._1.longDate,
	      periodicity = 1,
	      nbdays = nbDays,
	      value = v._2,
	      lastmodified = Some(java.util.Calendar.getInstance.getTime))
    	}
	
	
	outputln("source1:\t" + series1.size + " data from " + series1.firstKey.shortDate + " to " + series1.lastKey.shortDate)
	outputln("source2:\t" + series2.size + " data from " + series2.firstKey.shortDate + " to " + series2.lastKey.shortDate)
	outputln("result:\t" + resultseries.size + " data from " + resultseries.keySet.min.shortDate + " to " + resultseries.keySet.max.shortDate)
	outputln("errors:\t" + resultseries.filter(_._2.isNaN).size)
	pendingprice ++= result
	outputln("total price:\t" + pendingprice.size)
	printf(outputstring)
  }
    
}
