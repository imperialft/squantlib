package squantlib.model.discountcurve

import squantlib.parameter.yieldparameter.{YieldParameter, FlatVector}
import org.jquantlib.currencies.Currency
import org.jquantlib.time.{Date => JDate, Period => JPeriod, TimeUnit}
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions

/** 
 * Stores rate curve information and initialize discount curves as requested.
 * Require all discount curves to have same value date.
 * 
 * @param Map CurrencyId => DiscountCurve
 */
class DiscountCurveFactory(val curves:Map[String, DiscountableCurve], val cdscurves:Map[String, CDSCurve]) {

	val valuedate = curves.head._2.valuedate
	require(curves.forall(c => c._2.valuedate == valuedate))
	
	/** 
	 * USD
	 */
	val pivotcurrency = BasisSwapCurve.pivotcurrency.code
	
	/** 
	 * Currencies
	 */
	val currencies = curves.map { case (k, v) => v.currency }

	/** 
	 * Discounting Curves
	 */
	val discountingcurves = { curves.map{ case (cur, curve) => (cur, curve match { case r:RateCurve => r; case _ => null})}}.filter{case(k, c) => c != null}
	
	/**
	 * Stores already calculated discount curves.
	 * Assumption: for each key, value contains discount curve for both discount and pivot currency.
	 */
	var repository:Map[String, scala.collection.mutable.Map[String, DiscountCurve]] = Map.empty

	/**
	 * Returns discount curve. Discount currency is flat and same currency with given spread.
	 */
	def getdiscountcurve(ccy:String, spread:Double):DiscountCurve = getdiscountcurve(ccy, ccy, spread)
	
	/**
	 * Returns discount curve, flat spread.
	 */
	def getdiscountcurve(ccy:String, discountccy:String, spread:Double) : DiscountCurve = getdiscountcurve(ccy, discountccy, new FlatVector(valuedate, spread), null)

	/**
	 * Returns discount curve from stored cds parameter.
	 */
	def getdiscountcurve(ccy:String, cdsid:String) : DiscountCurve = if (cdscurves.keySet.contains(cdsid)) getdiscountcurve(ccy, cdscurves(cdsid).currency.code, cdscurves(cdsid).rate, cdsid) else null

	/**
	 * Returns discount curve from given CDS curve.
	 */
	def getdiscountcurve(ccy:String, spread:CDSCurve) : DiscountCurve = getdiscountcurve(ccy, spread.currency.code, spread.rate, null)
	
	/**
	 * Returns discount curve from full given parameter.
	 */
	private def getdiscountcurve(ccy:String, discountccy:String, spread:YieldParameter, cdsid:String) : DiscountCurve = 
	  if (contains(ccy, cdsid)) repository(cdsid)(ccy)
	  else {
	    val newcurve = ccy match {
		    case `discountccy` => { curves(ccy).getZC(spread) }
		    					
		    case `pivotcurrency` => { 
			    val zccurve = getdiscountcurve(discountccy, discountccy, spread, cdsid)
			    curves(ccy).getZC(ratecurve(discountccy), zccurve)
			    }
		      
		    case _ => { 
			    val pivotZC = getdiscountcurve(pivotcurrency, discountccy, spread, cdsid)
			    curves(ccy).getZC(ratecurve(pivotcurrency), pivotZC)
			    }
    	}
	    
	    if (cdsid != null) {
		    if (!repository.keySet.contains(cdsid)) repository += (cdsid -> scala.collection.mutable.Map(ccy -> newcurve))
		    else repository(cdsid) += (ccy -> newcurve)}
	    newcurve
	  }
	
	private def ratecurve(c:String):RateCurve = if (discountingcurves.keySet.contains(c)) discountingcurves(c) else throw new ClassCastException
	
	/**
	 * Checks whether the given curve is already calculated and stored in the repository.
	 */
	def contains(ccy:String, cdsid:String) = {
		repository.keySet.contains(cdsid) && repository(cdsid).keySet.contains(ccy)
	 }
	
	def describe = curves.map(c => c._2.describe + (if (discountingcurves.keySet.contains(c._1)) "(*)" else "") + 
				sys.props("line.separator")).mkString("") + "(*) Discounting curves"
	
    override def toString():String = "DiscountCurveFactory{" + curves.map(c => c._2).mkString(", ") + "}"
	
    def this(curves:Map[String, DiscountableCurve]) = this(curves, null)
}

