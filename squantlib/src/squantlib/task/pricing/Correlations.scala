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
import squantlib.database.QLConstructors._
import java.util.{Date => JavaDate}
import squantlib.initializer.RateConvention

object Correlations {
  
  private var pendingprice = new HashSet[Correlation] with SynchronizedSet[Correlation]
  private var storedts = new HashMap[String, TimeSeries[JavaDouble]] with SynchronizedMap[String, TimeSeries[JavaDouble]]
  
  def storedprice = pendingprice
   
  def push:Unit = {
    if (pendingprice.size != 0) {
		pendingprice.retain(!_.value.isNaN)
	    printf("Writing " + pendingprice.size + " items to Database...")
		val t1 = System.nanoTime
		DB.insertOrUpdate(pendingprice, false)
		val t2 = System.nanoTime
		printf("done (%.3f sec)\n".format(((t2 - t1)/1000000000.0)))
		pendingprice.clear
		}
	}
  
  def clear:Unit = pendingprice.clear
  
  def cleardb:Unit = DB.empty(DB.correlations)
  
  def lastDate:qlDate = DB.latestCorrelationDate 
  
  def defaultValueDate:qlDate = DB.latestPriceParam._2
  def defaultCurrencies:Set[String] = DB.getFXlist & RateConvention.currencies
  def defaultFXpairs:Set[(String, String)] = for(ccy1 <- defaultCurrencies; ccy2 <- defaultCurrencies if ccy1 >= ccy2) yield (ccy1, ccy2)
  def defaultBonds:Set[String] = DB.latestPrices.map(d => d.bondid)
  
  def pricefxfx(nbDays:Int):Unit = pricefxfx(nbDays, defaultValueDate)
  def pricefxfx(nbDays:Int, valueDate:qlDate):Unit = pricefxfx(defaultFXpairs, nbDays, defaultValueDate)
  
  def pricefxfx(fxpairs:Set[(String, String)], nbDays:Int, valuedate:qlDate):Unit = fxpairs.foreach{fx =>
	  price(underlying1 = "FX:" + fx._1 + "JPY",
    		source1 = fxTimeSeries(fx._1),
    		underlying2 = "FX:" + fx._2 + "JPY", 
    		source2 = fxTimeSeries(fx._2),
    		nbDays = nbDays, 
    		startDate = valuedate, 
    		endDate = valuedate)
  }
  
  def fxTimeSeries(ccy:String) = DB.getFXTimeSeries(ccy).toTimeSeries
  
  def bondCurrency(bondids:Set[String]):Map[String, String] = DB.getBonds(bondids).map(b => (b.id, b.currencyid)).toMap

  def bondTimeSeries(bondid:String, ccy:String):TimeSeries[JavaDouble] = { 
    val priceseries = DB.getPriceTimeSeries(bondid).toTimeSeries
    val fxid = "FX:" + ccy + "JPY"
    if (!storedts.keySet.contains(fxid)) storedts(fxid) = DB.getFXTimeSeries(ccy).toTimeSeries
    (priceseries.keySet & storedts(fxid).keySet).map(p => (p, priceseries(p) / storedts(fxid)(p))).toMap.toTimeSeries
  } 
  
  def pricefxbond(nbDays:Int):Unit = pricefxbond(defaultCurrencies, defaultBonds, nbDays, defaultValueDate)
  def pricefxbond(nbDays:Int, valuedate:qlDate):Unit = pricefxbond(defaultCurrencies, defaultBonds, nbDays, valuedate)
  
  def pricefxbond(bondlist:Set[String], ccylist:Set[String], nbDays:Int, valuedate:qlDate):Unit = {
    val ccybond = for(bond <- bondCurrency(bondlist); ccy <- ccylist) yield (bond, ccy)
    ccybond.foreach{case ((bond, ccy), fx) =>
	  price(underlying1 = "PRICE:" + bond,
			source1 = bondTimeSeries(bond, ccy),
			underlying2 = "FX:" + fx + "JPY", 
			source2 = fxTimeSeries(fx),
			nbDays = nbDays, 
			startDate = valuedate, 
			endDate = valuedate)
  }}
  
  
  def price(underlying1:String, source1: => TimeSeries[JavaDouble], underlying2:String, source2: => TimeSeries[JavaDouble], nbDays:Int = -1, startDate:qlDate, endDate:qlDate):Unit = {
    
    if (storedts.isEmpty || !storedts.keySet.contains(underlying1)) storedts(underlying1) = source1
    if (storedts.isEmpty || !storedts.keySet.contains(underlying2)) storedts(underlying2) = source2
    
//    val datastart =  startDate.sub(nbDays, )
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
	val ts1 = (series1.keySet & series2.keySet).map(d => (d, series1(d).doubleValue))
	val ts2 = (series1.keySet & series2.keySet).map(d => (d, series2(d).doubleValue))
	
    if (ts1.size < nbDays) {
      outputln("Error - Not enough elements: found " + ts1.size + " require " + nbDays)
      printf(outputstring)
      return
    }
	
	
	val sortedts1 = SortedMap(ts1.toSeq:_*)
	val sortedts2 = SortedMap(ts2.toSeq:_*)
	val resultseries = Correlation.calculate(sortedts1, sortedts2, nbDays).filter(c => ((c._1 ge startDate) && (c._1 le endDate)))
	
	if (resultseries == null || resultseries.size == 0)
	{
      outputln("Error - Empty Result")
      printf(outputstring)
      return
	}
	
	val currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
	
	val result = resultseries.map { v =>
    	new Correlation(
	      id = (underlying1 + ":" + underlying2 + ":" + ("%tY%<tm%<td" format v._1.longDate) + ":" + 1 + ":" + nbDays),
	      underlying1 = underlying1,
	      underlying2 = underlying2,
	      valuedate = v._1.longDate,
	      periodicity = 1,
	      nbdays = nbDays,
	      value = v._2,
	      lastmodified = Some(currenttime))
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
