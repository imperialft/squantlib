
import java.util.{Date => JavaDate, Calendar => JavaCalendar, GregorianCalendar => JavaGCalendar}
import squantlib.database.QLDB
import squantlib.database.QLConstructors._
import java.io.{FileOutputStream, PrintStream}

val enddate = new JavaGCalendar(2020, JavaCalendar.DECEMBER, 30).getTime

/** 
 * Update Coupons
*/
{
//	val bonds = DB.getCouponMissingBonds
//	val coupons = bonds.map(_.getCoupons).filter(_ != null).flatten
//	if (!coupons.isEmpty) DB.insertOrUpdate(coupons, false)
  println("Checking new Coupons")
  Coupons.initialize
  Coupons.update
  Coupons.push
}

//System.setErr(new PrintStream(new FileOutputStream("log/javaexceptions.log"))); BondPrices.updateJGBR_par(100);

/** 
 * Update Bond Price
*/

{
    println("\nBond Price:")
	val pricestream = new FileOutputStream("log/bondprice.log")
	System.setErr(new PrintStream(new FileOutputStream("log/javaexceptions.log")))
    
	val notpricedparam = BondPrices.notPricedParams
    if (notpricedparam.isEmpty) println("No new paramters")
	else {
	  println("Found new parameters") 
	  notpricedparam.foreach(println)
	  Console.withOut(pricestream){ BondPrices.updateNewDates(true) }
	}
	
    val notpricedbonds = BondPrices.notPricedBonds
    if (notpricedbonds.isEmpty) println("No new bonds")
    else {
      println("Found new bonds")
      notpricedbonds.foreach(println)
      Console.withOut(pricestream){ 
        BondPrices.updateNewBonds(true)
        }
    }
	
	System.setErr(System.err)
	pricestream.close
	BondPrices.push
	println("Price update complete")
}


/** 
 * Update ImpliedRates
*/
{
	val ratestream = new FileOutputStream("log/imprates.log")
	Console.withOut(ratestream) {
		ImpliedRates.update
	}
	ImpliedRates.push
}


/** 
 * Update Volatility
*/

{
	println("Volatilities:")
	val volstream = new java.io.FileOutputStream("log/volatility.log")
	val nbDays = Set(65, 130, 260)
	val annualDays = 260
	
	if (Volatilities.notPricedBonds.isEmpty) println("No new bonds:")
	else {
	  println("Found new bonds:")
	  Volatilities.notPricedBonds.foreach(println)
	  Console.withOut(volstream){ Volatilities.updateNewBonds(nbDays, null, null, annualDays)}
	}
	
	val (start, end) = Volatilities.notPricedDateRange
	if (end after start) {
	  println("Found new dates from " + start + " to " + end)
	  Console.withOut(volstream){ Volatilities.updateNewDates(nbDays, null, null, annualDays)}
	}
	
	volstream.close
	Volatilities.push
	println("Vol update complete")
}

/** 
 * Update Correlations
*/

{
	println("Correlation:")
	val nbDays = 130
	
	if (Correlations.updated) println("=> Correlation is up-to-date")
	else{
	  val correlstream = new java.io.FileOutputStream("log/correlation.log")
	  println("=> Update - Replace price " + Correlations.lastDate.orNull + " by " + Correlations.defaultValueDate)
	  DB.empty(DB.correlations)
	  Console.withOut(correlstream){
		Correlations.pricefxfx(nbDays)
		Correlations.pricefxbond(nbDays)
	  }
	  correlstream.close
	  Correlations.push
	}
	println("Correlation update complete")
}

/** 
 * Update Forward Prices
*/

{
	println("Forward Price:")
	if (ForwardPrices.updated) println("=> Foward Price is up-to-date")
	else {
		val forwardstream = new java.io.FileOutputStream("log/forwardprice.log")
		println("=> Update")
		DB.empty(DB.forwardprices)
		Console.withOut(forwardstream){ ForwardPrices.update }
		ForwardPrices.push
	}
	println("Forward price update complete")
}
