package squantlib.database.objectconstructor

import scala.collection.immutable.TreeMap

import squantlib.database.schemadefinitions.InputParameter
import squantlib.model.discountcurve.LiborDiscountCurve
import squantlib.model.currencies.RateConvention

import org.jquantlib.time.{Period => JPeriod, Date => JDate}
import org.jquantlib.currencies.Currency


object LiborDiscountCurveConstructor {
  import squantlib.parameter.yieldparameter.FlatVector
  
	val cashKey = "Cash"
	val swapKey = "Swap"
	val basisKey = "BasisSwap"
	val basis36Key = "BS3M6M"
	val fxKey = "FX"
	val pivotccy = "USD"
	  
  	def getCurves(valuedate:JDate, params:Set[InputParameter]):Map[(Currency, String), LiborDiscountCurve] = {
	  val conventions = RateConvention.getConvention.filter{case (k, v) => v.useratediscount }
      println(conventions.size + " conventions found ")
      conventions.foreach(c => println(c._1, c._2.currency.name))
      
  	  val dateassetgroup = params.groupBy(p => (p.asset, p.paramset)).filter{case((k1, k2), v) => conventions.keySet.contains(k1)}
      println(dateassetgroup.size + " date + asset grouping")
      dateassetgroup.foreach(c => println(c._1._1 + ", " + c._1._2 + ", " + c._2.size + " items"))
      
  	  val instrumentgroup:Map[(String, String), Map[String, Set[InputParameter]]] = dateassetgroup.map{ case (k, v) => (k, v.groupBy(p => p.instrument))} 
      println(instrumentgroup.size + " instrument grouping")
      instrumentgroup.foreach(c => println(c._1._1 + ", " + c._1._2 + ", " + c._2.size + " items" + c._2.keySet.map(d => d)))
  	  
  	  val nonemptyinstruments = instrumentgroup.filter{ case (k, v) => (v.keySet.contains(swapKey))}
  	  println(nonemptyinstruments.size + " nonempty instruments grouping")
      nonemptyinstruments.foreach(c => println(c._1._1 + ", " + c._1._2 + ", " + c._2.size + " items" + c._2.keySet.map(d => d)))
  	  
  	  nonemptyinstruments.map{ case ((k1, k2), v) => {
  	      println("initialising " + k1 + ":" + k2)
  		  val conv = conventions(k1)
  		  def toTreeMap(k:String) = TreeMap(v(k).toSeq.map(p => (new JPeriod(p.maturity), p.value)) :_*)
  		  val swapcurve = conv.swap_constructor(valuedate, toTreeMap(swapKey))
  		  val cashcurve = if (v.keySet.contains(cashKey)) conv.cash_constructor(valuedate, toTreeMap(cashKey))
  		  				  else conv.cash_constructor(new FlatVector(valuedate, swapcurve.rate.value(0)))
  		  val basiscurve = if (v.keySet.contains(basisKey)) conv.basis_constructor(valuedate, toTreeMap(basisKey)) else null
  		  val basis36curve = if (v.keySet.contains(basis36Key)) conv.basis36_constructor(valuedate, toTreeMap(basis36Key)) else null
  		  
  		  if (v.keySet.contains(fxKey)) ((conv.currency, k2), new LiborDiscountCurve(cashcurve, swapcurve, basiscurve, basis36curve, valuedate, v(fxKey).first.value))
  		  else ((conv.currency, k2), new LiborDiscountCurve(cashcurve, swapcurve, basiscurve, basis36curve, valuedate))
  	  	}
  	  }
  }

} 