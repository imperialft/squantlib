
/**
 * Creates factory from given paramset.
 * You must define following parameters in advance.
 *  val paramset:String => parameter id
 */


import java.util.{Date => JavaDate, Calendar => JavaCalendar, GregorianCalendar => JavaGCalendar, UUID}
import org.jquantlib.time.Date
import squantlib.task.pricing.Correlations
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
val pricedsets = DB.getPricedParamSets
val correldate = DB.getParamSets.filter(p => pricedsets.map(_._2).contains(p._2)).maxBy(_._2)
val resultparam = correldate._1
val resultstart = correldate._2
val resultend = resultstart
val datastart = new Date(1, 1, 2011)
val dataend = new Date(1, 1, 2020)
val currencies = DB.getFXlist & squantlib.initializer.RateConvention.getConvention.map(_._1).toSet
//val currencies = List("EUR", "USD", "BRL")
val fxpairs = currencies.map(fx1 => currencies.filter(_ >= fx1).map(fx2 => (fx1, fx2))).flatten
val nbDays = 130

val inputparamsfx:List[Correlparams] = fxpairs.map(fx => {
  val inputparam = new Correlparams(name1 = "FX:" + fx._1 + "JPY",
      				name2 = "FX:" + fx._2 + "JPY", 
      				nbDays = nbDays, 
      				datastart = datastart, 
      				dataend = dataend, 
      				resultstart = resultstart, 
      				resultend = resultend)
  inputparam.ts1 = () => {println("db access") ; DB.getFXTimeSeries(inputparam.datastart, inputparam.dataend, fx._1, "JPY").toTimeSeries}
  inputparam.ts2 = () => {println("db access") ; DB.getFXTimeSeries(inputparam.datastart, inputparam.dataend, fx._2, "JPY").toTimeSeries}
  inputparam}
  ).toList


//val latestparamset = DB.getPricedParamSets.maxBy(_._2)._1
val bondids = DB.getPriceByParamSet(resultparam).map(_.bondid)
val inputparamsfxbond:List[Correlparams] = bondids.map(bond => {
  currencies.map(fx => {
	  val inputparam = new Correlparams(name1 = "PRICE:" + bond, 
	      				name2 = "FX:" + fx + "JPY", 
	      				nbDays = nbDays, 
	      				datastart = datastart, 
	      				dataend = dataend, 
	      				resultstart = resultstart, 
	      				resultend = resultend)
	  inputparam.ts1 = () => {println("db access") ; DB.getJPYPriceTimeSeries(inputparam.datastart, inputparam.dataend, bond).toTimeSeries}
	  inputparam.ts2 = () => {println("db access") ; DB.getFXTimeSeries(inputparam.datastart, inputparam.dataend, fx, "JPY").toTimeSeries}
	  inputparam	  
  })
}).flatten.toList

val inputparamsbondbond:List[Correlparams] = bondids.map(bond1 => {
  bondids.map(bond2 => {
	  val inputparam = new Correlparams(name1 = "PRICE:" + bond1, 
	      				name2 = "PRICE:" + bond2, 
	      				nbDays = nbDays, 
	      				datastart = datastart, 
	      				dataend = dataend, 
	      				resultstart = resultstart, 
	      				resultend = resultend)
	  inputparam.ts1 = () => {println("db access") ; DB.getJPYPriceTimeSeries(inputparam.datastart, inputparam.dataend, bond1).toTimeSeries}
	  inputparam.ts2 = () => {println("db access") ; DB.getJPYPriceTimeSeries(inputparam.datastart, inputparam.dataend, bond2).toTimeSeries}
	  inputparam	  
  })
}).flatten.toList

val inputparams:List[Correlparams] = inputparamsfx ++ inputparamsfxbond ++ inputparamsbondbond

Console.withOut(ps) {
  	val starttime = System.nanoTime
	inputparams.par.foreach(p => {
	  Correlations.price(p.name1, p.ts1, p.name2, p.ts2, p.nbDays, p.resultstart, p.resultend)
	}
	)
  	val endtime = System.nanoTime
	println("Pricing completed: %.3f sec".format((endtime - starttime)/1000000000.0))
}

//CorrelPrice.pushdb

CorrelPrice.storedprice.foreach(p => println(p))