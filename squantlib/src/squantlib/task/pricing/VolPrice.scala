package squantlib.task.pricing

import squantlib.database._
import java.lang.{Double => JavaDouble}
import org.jquantlib.time.TimeSeries
import squantlib.database.schemadefinitions.Volatility
//import squantlib.model.timeseries.TsAnalysis._
import scala.collection.JavaConversions._ 
import org.jquantlib.time.{ Date => qlDate }
import scala.collection.mutable.{HashSet, SynchronizedSet, HashMap, SynchronizedMap}
import squantlib.math.timeseries.{Volatility => VolScala}
import scala.collection.SortedMap

object VolPrice {
  
  var storedprice = new HashSet[Volatility] with SynchronizedSet[Volatility]
  private var storedts = new HashMap[String, TimeSeries[JavaDouble]] with SynchronizedMap[String, TimeSeries[JavaDouble]]
  
//  def storedprice = pendingprice
   
  def push:Unit = {
    if (storedprice.size != 0) {
    	printf("Extracting valid price ..")
		storedprice.retain(!_.value.isNaN)
	    printf("Writing " + storedprice.size + " items to Database...")
		val t1 = System.nanoTime
		DB.insertOrReplace(storedprice)
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
	val resultseries = volresult.filter(c => ((c._1 ge startDate) && (c._1 le endDate)))
	
	val result = resultseries.map { v =>
    	new Volatility(
	      id = (underlying + ":" + ("%tY%<tm%<td" format v._1.longDate) + ":" + 1 + ":" + nbDays),
	      underlying = underlying,
	      valuedate = v._1.longDate,
	      periodicity = 1,
	      nbdays = nbDays,
	      value = v._2,
	      lastmodified = Some(java.util.Calendar.getInstance.getTime))
    	}
	
	outputln("source:\t" + series.size + " data from " + series.firstKey.shortDate + " to " + series.lastKey.shortDate)
	outputln("result:\t" + resultseries.size + " data from " + resultseries.keySet.min.shortDate + " to " + resultseries.keySet.max.shortDate)
	outputln("errors:\t" + resultseries.filter(_._2.isNaN).size)
	storedprice ++= result
	outputln("total price:\t" + storedprice.size)
	printf(outputstring)
  }
    
}


//package squantlib.task.pricing
//
//import squantlib.database._
//import java.lang.{Double => JavaDouble}
//import org.jquantlib.time.TimeSeries
//import squantlib.database.schemadefinitions.Volatility
//import squantlib.model.timeseries.TsAnalysis._
//import scala.collection.JavaConversions._
//import org.jquantlib.time.{ Date => qlDate }
//import scala.collection.mutable.ListBuffer
//
//object VolPrice {
//  
//  private var pendingprice = ListBuffer.empty[Volatility]
//   
//  def store(prices:Traversable[Volatility]):Unit = pendingprice.synchronized{
//    pendingprice ++= prices
//  }
//  
//  def storedprice = pendingprice
//   
//  def push:Unit = {
//    if (pendingprice.size != 0) {
//		val targetprices = pendingprice.filter(!_.value.isNaN)
//	    printf("Writing " + targetprices.size + " items to Database...")
//		val t1 = System.nanoTime
//		DB.insertOrReplace(targetprices)
//		val t2 = System.nanoTime
//		printf("done (%.3f sec)\n".format(((t2 - t1)/1000000000.0)))
//		pendingprice.clear
//		}
//	}
//  
//  def clear:Unit = pendingprice.clear
//  
//  def pricets(series:TimeSeries[JavaDouble], instrument:String, asset:String, maturity:String, nbDays:Int = -1, annualdays:Int = 260):Unit = {
//
//    var outputstring = ""
//    def output(s:String):Unit = { outputstring += s }
//    def outputln(s:String):Unit = { outputstring += s + "\n"}
//    
//	outputln("\n*** START OUTPUT VOL CALCULATION ***")
//	
//	/**
//	 * Creates factory from given paramset.
//	 */
//	val t1 = System.nanoTime
//	val resultseries = series.volatility(nbDays, annualdays)
//	val result = resultseries.map { v =>
//    	new Volatility(
//	      id = (instrument + ":" + asset + ":" + ("%tY%<tm%<td" format v._1.longDate) + ":" + (if (maturity == null) "" else maturity) + ":" + 1 + ":" + nbDays),
//	      instrument = instrument,
//	      asset = asset,
//	      maturity = maturity,
//	      valuedate = v._1.longDate,
//	      periodicity = 1,
//	      nbdays = nbDays,
//	      value = v._2,
//	      lastmodified = Some(java.util.Calendar.getInstance.getTime))
//    	}
//	val t2 = System.nanoTime
//	
//	outputln("source:\t" + series.size + " data from " + series.firstKey.shortDate + " to " + series.lastKey.shortDate)
//	outputln("result:\t" + resultseries.size + " data from " + resultseries.firstKey.shortDate + " to " + resultseries.lastKey.shortDate)
//	outputln("errors:\t" + resultseries.filter(_._2.isNaN).size)
//	printf(outputstring)
//	store(result)
//  }
//  
//  def price(fromDate:qlDate, toDate:qlDate, instrument:String, asset:String, maturity:String, nbDays:Int = -1, annualdays:Int = 260):Unit = 
//      pricets(QLDB.getTimeSeries(fromDate, toDate, instrument, asset, maturity), instrument, asset, maturity, nbDays, annualdays)
//    
//  def pricefx(fromDate:qlDate, toDate:qlDate, fx1:String, fx2:String, nbDays:Int = -1, annualdays:Int = 260):Unit = {
//    val dataset = QLDB.getFXTimeSeries(fromDate, toDate, fx1, fx2)
//    println("Found " + dataset.size + " data")
//    pricets(dataset, "FX", fx1+fx2, null, nbDays, annualdays)
//  }
//    
//  def pricecds(fromDate:qlDate, toDate:qlDate, currencyid:String, issuerid:String, maturity:String, nbDays:Int = -1, annualdays:Int = 260):Unit = 
//    pricets(QLDB.getCDSTimeSeries(fromDate, toDate, currencyid, issuerid, maturity), "CDS", issuerid+":"+currencyid, maturity, nbDays, annualdays)
//    
//}
