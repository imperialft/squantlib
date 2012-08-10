
/**
 * Creates factory from given paramset.
 * You must define following parameters in advance.
 *  val paramset:String => parameter id
 */


import java.util.{Date => JavaDate, Calendar => JavaCalendar, GregorianCalendar => JavaGCalendar, UUID}
import org.jquantlib.time.Date
import squantlib.task.pricing.CorrelPrice
import java.io.FileOutputStream
import squantlib.database.DB
import squantlib.database.QLConstructors._
import scala.collection.mutable.Map
import java.lang.{Double => JavaDouble}

class correlparams(val fx1:String, 
				    val fx2:String, 
				    val nbDays:Int, 
				    val datastart:Date, 
				    val dataend:Date, 
				    val resultstart:Date, 
				    val resultend:Date) {
  
		var ts1:() => TimeSeries[JavaDouble] = null
		var ts2:() => TimeSeries[JavaDouble] = null
}

val ps = new java.io.FileOutputStream("log/correlation.log")
val resultstart = DB.getParamSets.map(_._2).max
val resultend = resultstart
val datastart = new Date(1, 1, 2011)
val dataend = new Date(1, 1, 2020)
val currencies = DB.getFXList.toSet & squantlib.model.currencies.RateConvention.getConvention.map(_._1).toSet
val fxpairs = currencies.map(fx1 => currencies.map(fx2 => (fx1, fx2))).flatten
val nbDays = 130

val inputparamsfx = fxpairs.map(fx => {
  val newparam = new correlparams(fx1 = fx._1, 
      				fx2 = fx._2, 
      				nbDays = nbDays, 
      				datastart = datastart, 
      				dataend = dataend, 
      				resultstart = resultstart, 
      				resultend = resultend)
  newparam.ts1 = () => {println("db access") ; DB.getFXTimeSeries(newparam.datastart, newparam.dataend, fx._1).toTimeSeries}
  newparam.ts2 = () => {println("db access") ; DB.getFXTimeSeries(newparam.datastart, newparam.dataend, fx._2).toTimeSeries}
  newparam}
  )

//Console.withOut(ps) {
  	val starttime = System.nanoTime
	inputparams.par.foreach(fx => {
//	  val ts1 = () => {println("db access") ; DB.getFXTimeSeries(fx.datastart, fx.dataend, fx.fx1).toTimeSeries}
//	  val ts2 = () => {println("db access") ; DB.getFXTimeSeries(fx.datastart, fx.dataend, fx.fx2).toTimeSeries}
	  CorrelPrice.price(fx.fx1, fx.ts1, fx.fx2, fx.ts2, fx.nbDays, fx.resultstart, fx.resultend)
	}
	)
  	val endtime = System.nanoTime
	println("Pricing completed: %.3f sec".format((endtime - starttime)/1000000000.0))
//}

//BondPrice.pushdb

