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
	  
	  
	  
	/**
	 * Constructs LiborDiscountCurve from InputParameter per each combination of currency & paramset.
	 * Invalid input parameter sets are ignored.
	 * @param set of InputParameter
	 * @returns map from (Currency, ParamSet) to LiborDiscountCurve
	 */
  	def getcurves(params:Set[InputParameter]):Map[(String, String), LiborDiscountCurve] = {
	  val conventions:Map[String, RateConvention] = RateConvention.getConvention.filter{case (k, v) => v.useratediscount }
  	  val dateassetgroup = params.groupBy(p => (p.asset, p.paramset)).filter{case((k1, k2), v) => conventions.keySet.contains(k1)}
  	  val instrumentgroup = dateassetgroup.map{ case (k, v) => (k, v.groupBy(p => p.instrument))} 
  	  val nonemptyinstruments = instrumentgroup.filter{ case (k, v) => (v.keySet.contains(swapKey))}
  	  
  	  nonemptyinstruments.map{ case ((k1, k2), v) => {
  		  val conv = conventions(k1)
  		  val valuedate = new JDate(v(swapKey).head.paramdate)
  		  def toTreeMap(k:String) = TreeMap(v(k).toSeq.map(p => (new JPeriod(p.maturity), p.value)) :_*)
  		  val swapcurve = conv.swap_constructor(valuedate, toTreeMap(swapKey))
  		  val cashcurve = if (v.keySet.contains(cashKey)) conv.cash_constructor(valuedate, toTreeMap(cashKey))
  		  				  else conv.cash_constructor(new FlatVector(valuedate, swapcurve.rate.value(0)))
  		  val basiscurve = if (v.keySet.contains(basisKey)) conv.basis_constructor(valuedate, toTreeMap(basisKey)) else null
  		  val basis36curve = if (v.keySet.contains(basis36Key)) conv.basis36_constructor(valuedate, toTreeMap(basis36Key)) else null
  		  
  		  if (v.keySet.contains(fxKey)) ((k1, k2), new LiborDiscountCurve(cashcurve, swapcurve, basiscurve, basis36curve, v(fxKey).first.value))
  		  else ((k1, k2), new LiborDiscountCurve(cashcurve, swapcurve, basiscurve, basis36curve))
  	  	}}
  	}
  
	/**
	 * Constructs LiborDiscountCurve from InputParameter per each currency, only with specified paramset.
	 * Invalid input paramter sets or unmatched paramset ID are ignored.
	 * @param set of InputParameter
	 * @returns map from Currency to LiborDiscountCurve
	 */
  	def getcurves(params:Set[InputParameter], paramset:String):Map[String, LiborDiscountCurve] = 
  	  getcurves(params.filter(p => p.paramset == paramset)) map { case ((k1, k2), v) => (k1, v) }
  	
} 

