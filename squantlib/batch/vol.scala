
/**
 * Creates factory from given paramset.
 * You must define following parameters in advance.
 *  val paramset:String => parameter id
 */


import java.util.{Date => JavaDate, Calendar => JavaCalendar, GregorianCalendar => JavaGCalendar, UUID}
import org.jquantlib.time.Date
import squantlib.task.pricing.VolPrice
import java.io.FileOutputStream
import squantlib.database.DB
import squantlib.database.QLConstructors._
import scala.collection.mutable.Map
import java.lang.{Double => JavaDouble}

class Volparams(val name:String, 
				    val nbDays:Int, 
				    val datastart:Date, 
				    val dataend:Date, 
				    val resultstart:Date, 
				    val resultend:Date) {
  
		var ts:() => TimeSeries[JavaDouble] = null
}

val ps = new java.io.FileOutputStream("log/volatility.log")
val resultstart = new Date(1, 1, 2001)
val resultend = new Date(1, 1, 2020)
val datastart = new Date(1, 1, 2000)
val dataend = new Date(1, 1, 2020)
val fxpairs = DB.getFXList.map(fx => (fx, "JPY"))
val nbDays = List(65, 130, 260)

val inputparamsfx:List[Volparams] = fxpairs.map(fx => {
  nbDays.map(d => {
	  val inputparam = new Volparams(name = ("FX:" + fx._1 + "JPY"),
	      				nbDays = d, 
	      				datastart = datastart, 
	      				dataend = dataend, 
	      				resultstart = resultstart, 
	      				resultend = resultend)
	  inputparam.ts = () => {println("db access") ; DB.getFXTimeSeries(inputparam.datastart, inputparam.dataend, fx._1).toTimeSeries}
	  inputparam
	  })
	}
  ).flatten.toList

val latestparamset = DB.getPricedParamsets.maxBy(_._2)._1
val bondids = DB.getPriceByParamset(latestparamset).map(_.bondid)
val inputparamsbond:List[Volparams] = bondids.map(bond => {
  nbDays.map(d => {
	  val inputparam = new Volparams(name = "PRICE:" + bond, 
	      				nbDays = d, 
	      				datastart = datastart, 
	      				dataend = dataend, 
	      				resultstart = resultstart, 
	      				resultend = resultend)
	  inputparam.ts = () => {println("db access") ; DB.getJPYPriceTimeSeries(inputparam.datastart, inputparam.dataend, bond).toTimeSeries}
	  inputparam
	})
}).flatten.toList

val inputparams:List[Volparams] = inputparamsfx ++ inputparamsbond

Console.withOut(ps) {
  	val starttime = System.nanoTime
	inputparams.par.foreach(p => {
	  VolPrice.price(p.name, p.ts, p.nbDays, p.resultstart, p.resultend)
	}
	)
  	val endtime = System.nanoTime
	println("Pricing completed: %.3f sec".format((endtime - starttime)/1000000000.0))
}

//BondPrice.pushdb





//
///**
// * Creates factory from given paramset.
// * You must define following parameters in advance.
// *  val paramset:String => parameter id
// */
//
//
//import java.util.{Date => JavaDate, Calendar => JavaCalendar, GregorianCalendar => JavaGCalendar, UUID}
//import org.jquantlib.time.Date
//import squantlib.task.pricing.BondPrice
//import java.io.FileOutputStream
//import squantlib.database.DB
//import squantlib.database.QLConstructors._
//
//class FXparam (val fx1:String, val fx2:String, val nbDays:Int, val startdate:Date, val enddate:Date)
//
//val ps = new java.io.FileOutputStream("log/volprice.log")
//val startdate = new Date(1, 1, 2000)
//val enddate = new Date(1, 1, 2020)
//val fxpairs = DB.getFXList.map(fx => (fx, "JPY"))
//val nbDays = List(65, 130, 260)
//val fxparams = fxpairs.map(fx => nbDays.map(d => new FXparam(fx._1, fx._2, d, startdate, enddate))).flatten
//
//Console.withOut(ps) {
//  	val starttime = System.nanoTime
//	fxparams.par.foreach(p => 
////		VolPrice.pricefx(startdate, enddate, fxd._1._1, fxd._1._2, fxd._2)
//		VolPrice.pricefx(p.startdate, p.enddate, p.fx1, p.fx2, p.nbDays)
//	)
//  	val endtime = System.nanoTime
//	println("Pricing completed: %.3f sec".format((endtime - starttime)/1000000000.0))
//}
//
////BondPrice.pushdb
//
