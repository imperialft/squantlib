
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

class Correlparams(val name1:String, 
				    val name2:String, 
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

val inputparamsfx:List[Correlparams] = fxpairs.map(fx => {
  val inputparam = new Correlparams(name1 = "FX:" + fx._1 + "JPY",
      				name2 = "FX:" + fx._2 + "JPY", 
      				nbDays = nbDays, 
      				datastart = datastart, 
      				dataend = dataend, 
      				resultstart = resultstart, 
      				resultend = resultend)
  inputparam.ts1 = () => {println("db access") ; DB.getFXTimeSeries(inputparam.datastart, inputparam.dataend, fx._1).toTimeSeries}
  inputparam.ts2 = () => {println("db access") ; DB.getFXTimeSeries(inputparam.datastart, inputparam.dataend, fx._2).toTimeSeries}
  inputparam}
  ).toList


val latestparamset = DB.getPricedParamsets.maxBy(_._2)._1
val bondids = DB.getPriceByParamset(latestparamset).map(_.bondid)
val inputparamsbond:List[Correlparams] = bondids.map(bond => {
  currencies.map(fx => {
	  val inputparam = new Correlparams(name1 = "PRICE:" + bond, 
	      				name2 = "FX:" + fx + "JPY", 
	      				nbDays = nbDays, 
	      				datastart = datastart, 
	      				dataend = dataend, 
	      				resultstart = resultstart, 
	      				resultend = resultend)
	  inputparam.ts1 = () => {println("db access") ; DB.getJPYPriceTimeSeries(inputparam.datastart, inputparam.dataend, bond).toTimeSeries}
	  inputparam.ts2 = () => {println("db access") ; DB.getFXTimeSeries(inputparam.datastart, inputparam.dataend, fx).toTimeSeries}
	  inputparam	  
  })
}).flatten.toList

val inputparams:List[Correlparams] = inputparamsfx ++ inputparamsbond
  
Console.withOut(ps) {
  	val starttime = System.nanoTime
	inputparams.par.foreach(p => {
	  CorrelPrice.price(p.name1, p.ts1, p.name2, p.ts2, p.nbDays, p.resultstart, p.resultend)
	}
	)
  	val endtime = System.nanoTime
	println("Pricing completed: %.3f sec".format((endtime - starttime)/1000000000.0))
}

//BondPrice.pushdb

