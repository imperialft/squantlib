package squantlib.task.pricing

import squantlib.database.{DB, QLDB}
import java.lang.{Double => JavaDouble}
import org.jquantlib.time.TimeSeries
import squantlib.database.schemadefinitions.ImpliedRate
import org.jquantlib.time.{ Date => qlDate, Period => qlPeriod }
import scala.collection.mutable.{HashSet, SynchronizedSet, HashMap, SynchronizedMap}
import org.jquantlib.instruments.{Bond => qlBond}
import squantlib.initializer.{Calendars, RateConvention}

object ImpliedRates { 
  
  var storedprice = new HashSet[ImpliedRate] with SynchronizedSet[ImpliedRate]
  
  val discountcurve = "JPY"
  val discountspread = 0.0
  val maturities = Set("3M", "6M", "1Y", "3Y", "5Y", "10Y", "15Y", "20Y", "30Y")
  def maturitydates(d:qlDate) = maturities.map(m => (m, new qlPeriod(m).days(d)))
  
  def push:Unit =  {
    if (storedprice.size != 0) {
    	printf("Extracting valid price ..")
		storedprice.retain(!_.value.isNaN)
	    printf("Writing " + storedprice.size + " items to Database...")
		val t1 = System.nanoTime
		val rows = DB.insertOrUpdate(storedprice, true)
		val t2 = System.nanoTime
		printf("done " + rows + " rows inserted (%.3f sec)\n".format(((t2 - t1)/1000000000.0)))
		storedprice.clear
		}
	}
  
  def clear:Unit = storedprice.clear
  
  def allParamSets:Set[String] = DB.getParamSets.map(_._1)
  def pricedParamSets:Set[String] = DB.getImpliedRateParams
  def updated:Boolean = unpricedParamSets.isEmpty
  def unpricedParamSets:Set[String] = allParamSets -- pricedParamSets
  
  def update:Unit = {
    unpricedParamSets.foreach(price)
  }
  
  def update_par:Unit = {
    unpricedParamSets.par.foreach(price)
  }

  def price(paramset:String):Unit = {
    println("PARAMSET:" + paramset)
	val factory = QLDB.getDiscountCurveFactory(paramset).orNull
	if (factory == null || factory.curves.size == 0) {
	  println("Curve not found")
	  return
	}
	  
	println("valuedate :\t" + factory.valuedate.shortDate)
	println("\n* Market *")
	println(factory.describe)
	
	val dates = maturitydates(factory.valuedate)
	val currenttime = new java.sql.Timestamp(java.util.Calendar.getInstance.getTime.getTime)
	
	val impliedrates:Set[ImpliedRate] = factory.curvelist.map ( c => {
	  val zccurve = try factory.getdiscountcurve(c, discountcurve, discountspread).orNull catch { case e => null}
	  
	  if (zccurve == null) {
	    println("failed to initialise curve : " + c)
	  }
	  dates.map { case (m, d) => {
	    printf(c + " " + m + " " + d + " ")
	    val zcvalue = try zccurve(d) catch { case e => Double.NaN }
	    val zcrate = if (zcvalue.isNaN) Double.NaN else - Math.log(zcvalue) / (d / 365.00)
	    printf(zcvalue + " / " + zcrate + "\n")
		new ImpliedRate (
		    id = "CONTINUOUS:" + c + ":" + paramset + ":" + m,
		    paramset = paramset,
			paramdate = factory.valuedate.longDate,
			instrument = "CONTINUOUS",
			asset = c,
			maturity = m,
			value = zcrate,
			comment = null,
			created = Some(currenttime)
		    )
	  }}.toSet
	}).flatten;
	
	println("Done for paramset " + paramset)
	storedprice ++= impliedrates
  }
    
}

