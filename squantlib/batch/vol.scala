

import java.util.{Date => JavaDate, Calendar => JavaCalendar, GregorianCalendar => JavaGCalendar, UUID}
import org.jquantlib.time.Date
import squantlib.task.pricing.Volatilities
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
var resultstart = new Date(1, 1, 2001)
var resultend = new Date(1, 1, 2020)
val datastart = new Date(1, 1, 2000)
val dataend = new Date(1, 1, 2020)
val fxpairs = DB.getFXlist.map(fx => (fx, "JPY"))
//val fxpairs = List(("EUR", "JPY"), ("USD", "JPY"))
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
  

val latestparamset = DB.getPricedParamSets.maxBy(_._2)._1
val bondids = DB.getPriceByParamSet(latestparamset).map(_.bondid)

val bondset = DB.getBonds.map(b => (b.id, b)).toMap

val inputparamsbond:List[Volparams] = bondids.map(bond => {
  nbDays.map(d => {
	  val bondissue = bondset(bond).issuedate
	  val inputparam = new Volparams(name = "PRICE:" + bond, 
	      				nbDays = d, 
	      				datastart = datastart, 
	      				dataend = dataend, 
	      				resultstart = if (bondissue ge resultstart) bondissue else resultstart,
	      				resultend = resultend)
	  inputparam.ts = () => {println("db access") ; DB.getJPYPriceTimeSeries(inputparam.datastart, inputparam.dataend, bond, 1.00).toTimeSeries}
	  inputparam
	})
}).flatten.toList

val inputparams:List[Volparams] = inputparamsfx ++ inputparamsbond

Console.withOut(ps) {
  	val starttime = System.nanoTime
  	println("Start Calculation : ")
	inputparams.par.foreach(p => {
	  Volatilities.price(p.name, p.ts, p.nbDays, p.resultstart, p.resultend)
	}
	)
  	val endtime = System.nanoTime
	println("Pricing completed: %.3f sec".format((endtime - starttime)/1000000000.0))
}

Volatilities.push

//BondPrice.pushdb




