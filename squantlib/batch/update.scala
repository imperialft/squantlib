
import java.util.{Date => JavaDate, Calendar => JavaCalendar, GregorianCalendar => JavaGCalendar}
import squantlib.database.QLDB
import squantlib.database.QLConstructors._
import java.io.{FileOutputStream, PrintStream}

val enddate = new JavaGCalendar(2020, JavaCalendar.DECEMBER, 30).getTime


/** 
 * Update Bond Price
*/

{
	println("Price: new parameters")
	BondPrices.notPricedParams.foreach(println)
	
	println("Price: new bonds")
	BondPrices.notPricedBonds.foreach(println)
	 
	val pricestream = new FileOutputStream("log/bondprice.log")
	
	System.setErr(new PrintStream(new FileOutputStream("log/javaexceptions.log")))
	Console.withOut(pricestream){
	  BondPrices.updateNewDates_par
	  BondPrices.updateNewBonds_par
	}
	System.setErr(System.err)
	pricestream.close
	
	BondPrices.push
}

/** 
 * Update Coupons
*/
{
	val bonds = DB.getCouponMissingBonds
	val coupons = bonds.map(_.getCoupons).filter(_ != null).flatten
	if (!coupons.isEmpty) DB.insertOrUpdate(coupons, false)
}

/** 
 * Update Volatility
*/

{
	val volstream = new java.io.FileOutputStream("log/volatility.log")
	val startdate = new Date(1, 1, 2000)
	val (lastparam, enddate) = DB.latestPriceParam
	val nbDays = Set(65, 130, 260)
	val annualDays = 260
	
	println("Volatilities: new bonds")
	Volatilities.notPricedBonds.foreach(println)
	
	println("Volatilities: new dates")
	val (start, end) = Volatilities.notPricedDateRange
	if (end after start) println("from " + start.shortDate + " to " + end.shortDate)
	
	Console.withOut(volstream){
	  Volatilities.updateNewDates(nbDays, null, null, annualDays)
	  Volatilities.updateNewBonds(nbDays, startdate, enddate, annualDays)
	}
	volstream.close
	Volatilities.push
	
}

/** 
 * Update Correlations
*/

{
	val nbDays = 130
	val lastcorrel = Correlations.lastDate
	val currentvd = Correlations.defaultValueDate
	val correlstream = new java.io.FileOutputStream("log/correlation.log")
	
	println("Correlation: last priced: " + lastcorrel.shortDate + " current vd: " + currentvd.shortDate)
	
	if (currentvd gt lastcorrel) {
	  DB.empty(DB.correlations)
	  Console.withOut(correlstream){
		println("=> Update")
		Correlations.pricefxfx(nbDays)
		Correlations.pricefxbond(nbDays)
	  }
	  correlstream.close
	  Correlations.push
	}
}

/** 
 * Update Forward Prices
*/

{
	if (ForwardPrices.updated) println("Foward Price is up-to-date")
	else {
		val forwardstream = new java.io.FileOutputStream("log/forwardprice.log")
		println("Update Forward Price")
		DB.empty(DB.forwardprices)
		Console.withOut(forwardstream){
			ForwardPrices.update
			ForwardPrices.push		  
		}
		ForwardPrices.push
	}
}
