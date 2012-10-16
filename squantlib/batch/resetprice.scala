import java.io.{FileOutputStream, PrintStream}
import BondPrices._

val dates = List(
			new Date(1, 1, 2003), 
			new Date(1, 1, 2006), 
			new Date(1, 1, 2008),
			new Date(1, 1, 2009),
			new Date(1, 1, 2010),
			new Date(1, 1, 2011),
			new Date(1, 1, 2012),
			new Date(1, 1, 2013)
			)
			
System.setErr(new PrintStream(new FileOutputStream("log/javaexceptions.log")))

for (i <- 0 to dates.size - 2) {
	val fromDate = dates(i).longDate
	val toDate = dates(i+1).longDate
	val params = notPricedParams(fromDate, toDate)
	BondPrices.updateNewDates_noJGB(fromDate, toDate, true)
	BondPrices.push
}

