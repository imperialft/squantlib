
/**
 * Creates factory from given paramset.
 * You must define following parameters in advance.
 *  val paramset:String => parameter id
 */


import java.util.{Date => JavaDate, Calendar => JavaCalendar, GregorianCalendar => JavaGCalendar, UUID}
import squantlib.task.pricing.BondPrice
import java.io.FileOutputStream
import squantlib.database.DB

val ps = new java.io.FileOutputStream("log/bondprice.log")
val startdate = new JavaGCalendar(2000, JavaCalendar.JANUARY, 1).getTime
val enddate = new JavaGCalendar(2020, JavaCalendar.JANUARY, 1).getTime
val fxpairs = List(("USD", "JPY"),
					("EUR", "JPY"),
					("AUD", "JPY"))

Console.withOut(ps) {
  	val starttime = System.nanoTime
	fxpairs.par.foreach(d => VolPrice.price())
  	val endtime = System.nanoTime
	println("Pricing completed: %.3f sec".format((endtime - starttime)/1000000000.0))
}

//BondPrice.pushdb

