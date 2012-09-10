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
import scala.collection.{SortedMap, SortedSet}
import squantlib.database.QLConstructors._
import java.util.{Date => JavaDate}
import squantlib.database.DB

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
  
  def pricefx(currencies:Set[String], nbDays:Set[Int], startDate:qlDate, endDate:qlDate, annualDays:Int = 260) = {
    val pricesets = for (ccy <- currencies; d <- nbDays) yield (ccy, d)
    pricesets.par.foreach { case (ccy, d) => 
      price("FX:" + ccy + "JPY", fxTimeSeries(ccy), d, startDate, endDate, annualDays)
  }}
  
  def pricedBonds:Set[String] = DB.getVolatilityBondUnderlyings
  def notPricedBonds:Set[String] = (DB.latestPrices.map(_.bondid) -- pricedBonds).map(_.toString)
  def notPricedDateRange:(JavaDate, JavaDate) = (
      if (DB.latestVolatilityDate != null) DB.latestVolatilityDate else DB.getPricedParamSets.map(_._2).min, 
      DB.latestPriceParam._2)
  
  def updateNewDates(nbDays:Set[Int], bondids:Set[String] = null, fxids:Set[String] = null, annualDays:Int = 260) = {
    val bonds:Set[String] = if (bondids == null) pricedBonds else bondids
    val fxs:Set[String] = if (fxids == null) DB.getFXlist else fxids
    val (startdate, enddate) = notPricedDateRange
    if (enddate.after(startdate) && !bonds.isEmpty) pricebond(bonds, nbDays, startdate, enddate, annualDays)
    if (enddate.after(startdate) && !fxs.isEmpty) pricefx(fxs, nbDays, startdate, enddate, annualDays)
  }
  
  def updateNewBonds(nbDays:Set[Int], startDate:qlDate, endDate:qlDate, annualDays:Int = 260) = 
    if (!notPricedBonds.isEmpty) pricebond(notPricedBonds, nbDays, startDate, endDate, annualDays)
    
  def pricebond(bondids:Set[String], nbDays:Set[Int], startDate:qlDate = null, endDate:qlDate = null, annualDays:Int = 260) = {
    val bondfx = bondCurrency(bondids).toSet
    val pricesets = for (id <- bondfx; d <- nbDays) yield (id, d)
	val sdate = if (startDate == null) new qlDate(1, 1, 2000) else startDate
	val edate:qlDate = if (endDate == null) DB.latestPriceParam._2 else endDate
    
    pricesets.par.foreach { case ((bondid, ccy), d) => 
      price("PRICE:" + bondid, bondTimeSeries(bondid, ccy), d, sdate, edate, annualDays)
    }
  }

  def fxTimeSeries(ccy:String) = DB.getFXTimeSeries(ccy).toTimeSeries
  
  def bondTimeSeries(bondid:String, ccy:String):TimeSeries[JavaDouble] = { 
    val priceseries = DB.getPriceTimeSeries(bondid).toTimeSeries
    val fxid = "FX:" + ccy + "JPY"
    if (!storedts.keySet.contains(fxid)) storedts(fxid) = DB.getFXTimeSeries(ccy).toTimeSeries
    (priceseries.keySet & storedts(fxid).keySet).map(p => (p, priceseries(p) * storedts(fxid)(p))).toMap.toTimeSeries
  }
  
  def bondCurrency(bondids:Set[String]):Map[String, String] = DB.getBonds(bondids).map(b => (b.id, b.currencyid)).toMap

  def price(underlying:String, source: => TimeSeries[JavaDouble], nbDays:Int = -1, startDate:qlDate, endDate:qlDate, annualDays:Int = 260):Unit = {
    
    if (storedts.isEmpty || !storedts.keySet.contains(underlying)) storedts(underlying) = source
      
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
	
	val targetkeys = {
	  val sortedkeys = SortedSet(series.keySet.toSeq :_*)
	  sortedkeys.takeRight(sortedkeys.count(startDate le) + nbDays)
	}
	
    if (targetkeys.size < nbDays) {
      outputln("Error - Not enough elements: found " + targetkeys.size + " require " + nbDays)
      printf(outputstring)
      return
    }
    
	val sortedts = SortedMap(series.filterKeys(targetkeys contains).mapValues(_.doubleValue).toSeq:_*)
	
//	val ts = series.map{ case (s, t) => (s, t.doubleValue)}
//	val sortedts = SortedMap(ts.toSeq:_*)
	val volresult = VolScala.calculate(sortedts, nbDays, annualDays)
	val resultseries = volresult.filter{case (d, v) => ((d ge startDate) && (d le endDate) && (!v.isNaN))}
	val currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
	
	val result = resultseries.map { case (d, v) =>
    	new Volatility(
	      id = (underlying + ":" + ("%tY%<tm%<td" format d.longDate) + ":" + 1 + ":" + nbDays),
	      underlying = underlying,
	      valuedate = d.longDate,
	      periodicity = 1,
	      nbdays = nbDays,
	      value = v,
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
