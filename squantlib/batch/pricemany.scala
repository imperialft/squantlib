
/**
 * Creates factory from given paramset.
 * You must define following parameters in advance.
 *  val paramset:String => parameter id
 */


import java.util.{Date => JavaDate, Calendar => JavaCalendar, GregorianCalendar => JavaGCalendar, UUID}
import squantlib.task.pricing.BondPrices
import java.io.FileOutputStream

val ps = new java.io.FileOutputStream("log/bondprice.log")
val startdate = new JavaGCalendar(2006, JavaCalendar.JANUARY, 1).getTime
val enddate = new JavaGCalendar(2008, JavaCalendar.DECEMBER, 30).getTime
//val dates = DB.getParamSets(startdate, enddate).toList sortBy(_._2)
val dates = DB.getParamSets(startdate, enddate).toSet

//Console.withOut(ps) {
  	println("Start pricing " + dates.size + " items\n")
  	val starttime = System.nanoTime
	dates.par.foreach(d => BondPrices.price(d._1))
  	val endtime = System.nanoTime
	println("Pricing completed: %.3f sec".format((endtime - starttime)/1000000000.0))
//}

BondPrices.push

