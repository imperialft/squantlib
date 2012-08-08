package squantlib.task.pricing

import squantlib.database._
import java.lang.{Double => JavaDouble}
import org.jquantlib.time.TimeSeries
import squantlib.database.schemadefinitions.Volatility
import squantlib.model.timeseries.TsAnalysis._
import scala.collection.JavaConversions._
import org.jquantlib.time.{ Date => qlDate }


object VolPrice {
  
  private var pendingprice = scala.collection.mutable.ListBuffer.empty[Volatility]
  
  def store(prices:Traversable[Volatility]):Unit = pendingprice.synchronized{
    pendingprice ++= prices
  }
  
  def storedprice = pendingprice
   
  def push:Unit = {
    if (pendingprice.size != 0) {
		val targetprices = pendingprice.filter(p => (!p.value.isNaN))
	    printf("Writing " + targetprices.size + " items to Database...")
		val t1 = System.nanoTime
		DB.insertOrReplace(targetprices)
		val t2 = System.nanoTime
		printf("done (%.3f sec)\n".format(((t2 - t1)/1000000000.0)))
		pendingprice.clear
		}
	}
  
  def pricets(series:TimeSeries[JavaDouble], instrument:String, asset:String, maturity:String, nbDays:Int = -1, annualdays:Int = 260):Unit = {

    var outputstring = ""
    def output(s:String):Unit = { outputstring += s }
    def outputln(s:String):Unit = { outputstring += s + "\n"}
    
	outputln("\n*** START OUTPUT VOL CALCULATION ***")
	
	/**
	 * Creates factory from given paramset.
	 */
	val t1 = System.nanoTime
	val resultseries = series.volatility(nbDays, annualdays)
	val result = resultseries.map { v =>
    	new Volatility(
	      id = (instrument + ":" + v._1.shortDate + ":" + asset + ":" + maturity + ":" + nbDays),
	      instrument = instrument,
	      asset = asset,
	      maturity = maturity,
	      valuedate = v._1.longDate,
	      periodicity = 1,
	      nbdays = nbDays,
	      value = v._2,
	      lastmodified = Some(java.util.Calendar.getInstance.getTime))
    	}
	val t2 = System.nanoTime
	
	outputln("source:\t" + series.size + " data from " + series.firstKey.shortDate + " to " + series.lastKey.shortDate)
	outputln("result:\t" + resultseries.size + " data from " + resultseries.firstKey.shortDate + " to " + resultseries.lastKey.shortDate)
	outputln("errors:\t" + resultseries.filter(_._2.isNaN))
	printf(outputstring)
	store(result)
  }
  
  def price(fromDate:qlDate, toDate:qlDate, instrument:String, asset:String, maturity:String, nbDays:Int = -1, annualdays:Int = 260):Unit = 
      pricets(QLDB.getTimeSeries(fromDate, toDate, instrument, asset, maturity), instrument, asset, maturity, nbDays, annualdays)
    
  def pricefx(fromDate:qlDate, toDate:qlDate, fx1:String, fx2:String, nbDays:Int = -1, annualdays:Int = 260):Unit = 
    pricets(QLDB.getFXTimeSeries(fromDate, toDate, fx1, fx2), "FX", fx1+fx2, null, nbDays, annualdays)
    
  def pricecds(fromDate:qlDate, toDate:qlDate, currencyid:String, issuerid:String, maturity:String, nbDays:Int = -1, annualdays:Int = 260):Unit = 
    pricets(QLDB.getCDSTimeSeries(fromDate, toDate, currencyid, issuerid, maturity), "CDS", issuerid+":"+currencyid, maturity, nbDays, annualdays)
    
}
