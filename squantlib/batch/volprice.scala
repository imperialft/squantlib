
/**
 * Creates factory from given paramset.
 * You must define following parameters in advance.
 *  val paramset:String => parameter id
 */


import java.util.{Date => JavaDate, Calendar => JavaCalendar, GregorianCalendar => JavaGCalendar, UUID}
import org.jquantlib.time.Date
import squantlib.task.pricing.BondPrice
import java.io.FileOutputStream
import squantlib.database.DB
import squantlib.database.QLConstructors._

val ps = new java.io.FileOutputStream("log/volprice.log")
//val startdate = new JavaGCalendar(2000, JavaCalendar.JANUARY, 1).getTime
//val enddate = new JavaGCalendar(2020, JavaCalendar.JANUARY, 1).getTime
val startdate = new Date(1, 1, 2000)
val enddate = new Date(1, 1, 2020)
val fxpairs = DB.getFXList.map(fx => (fx, "JPY"))
val nbDays = List(65, 130, 260)
val fxday = fxpairs.map(fx => nbDays.map(d => (fx, d))).flatten

Console.withOut(ps) {
  	val starttime = System.nanoTime
	fxday.foreach(fxd => 
		VolPrice.pricefx(startdate, enddate, fxd._1._1, fxd._1._2, fxd._2)
	)
  	val endtime = System.nanoTime
	println("Pricing completed: %.3f sec".format((endtime - starttime)/1000000000.0))
}

//BondPrice.pushdb

