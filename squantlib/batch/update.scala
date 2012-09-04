
import java.util.{Date => JavaDate, Calendar => JavaCalendar, GregorianCalendar => JavaGCalendar}
import squantlib.database.QLDB
import squantlib.database.QLConstructors._
import java.io.{FileOutputStream, PrintStream}

val enddate = new JavaGCalendar(2020, JavaCalendar.DECEMBER, 30).getTime


/** 
 * Update Bond Price
 * - New Pricing Dates
 * - New Bonds
*/

{
	println("Price: new parameters")
	BondPrices.notPricedParams.foreach(println)
	
	println("Price: new bonds")
	BondPrices.notPricedBonds.foreach(println)
	 
	val pricestream = new FileOutputStream("log/bondprice.log")
	
	System.setErr(new PrintStream(new FileOutputStream("log/javaexceptions.log")))
	Console.withOut(pricestream){
	  BondPrices.updateNewDates
	  BondPrices.updateNewBonds
	}
	System.setErr(System.err)
	
	BondPrices.push
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
	
	Volatilities.push
	
}

/** 
 * Update Correlations
*/

{
	val nbDays = 130
	val lastcorrel = Correlations.lastDate
	val currentvd = Correlations.defaultValueDate
	
	println("Correlation: last priced: " + lastcorrel.shortDate + " current vd: " + currentvd.shortDate)
	
	if (currentvd gt lastcorrel)
	{
		println("=> Update")
		Correlations.pricefxfx(nbDays)
		Correlations.pricefxbond(nbDays)
	}
  
}

