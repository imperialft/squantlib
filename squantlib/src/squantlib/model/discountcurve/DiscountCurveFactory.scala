package squantlib.model.discountcurve

import squantlib.parameter.yieldparameter.YieldParameter
import squantlib.parameter.yieldparameter.FlatVector
import org.jquantlib.currencies.Currency
import org.jquantlib.time.{Date => JDate}
import org.jquantlib.time.{Period => JPeriod}
import org.jquantlib.time.TimeUnit
import scala.collection.mutable.HashMap


/** 
 * Stores rate curve information and initialize discount curves as requested.
 */
class DiscountCurveFactory(val curves:Map[Currency, DiscountableCurve], val valuedate:JDate) {

	/** 
	 * USD
	 */
	val pivotcurrency = BasisSwapCurve.pivotcurrency
	
	/**
	 * Stores already calculated discount curves.
	 * Assumption: for each key, value contains discount curve for both discount and pivot currency.
	 */
	var repository:Map[(Currency, Double), scala.collection.mutable.Map[Currency, DiscountCurve]] = Map.empty

	/**
	 * Returns discount curve. Discount currency is the same currency with given spread.
	 */
	def discountcurve(ccy:Currency, spread:Double):DiscountCurve = discountcurve(ccy, ccy, spread)
	
	/**
	 * Returns discount curve. Discount currency is as specified with zero spread.
	 */
	def discountcurve(ccy:Currency, discountccy:Currency):DiscountCurve = discountcurve(ccy, discountccy, 0.0)

	/**
	 * Returns discount curve. Discount currency is as specified with given spread.
	 */
	def discountcurve(ccy:Currency, discountccy:Currency, spread:Double) : DiscountCurve = 
	  if (contains(ccy, discountccy, spread)) repository(Pair(discountccy, spread))(ccy)
	  else {
	    val key = (discountccy, spread)
	    val newcurve = ccy match {
		    case `discountccy` => { 
		      val rate = ratecurve(curves(ccy))
		      val flatvector = new FlatVector(valuedate, Map(new JPeriod(6, TimeUnit.Months) -> spread))
		      val zccurve = curves(ccy).getZC(flatvector)
		      zccurve
		      }
		    					
		    case `pivotcurrency` => { 
		        val discountrate = ratecurve(curves(discountccy))
			    val zccurve = discountcurve(discountccy, discountccy, spread)
			    curves(ccy).getZC(discountrate, zccurve)
			    }
		      
		    case _ => { 
		        val pivotrate = ratecurve(curves(pivotcurrency))
			    val pivotZC = discountcurve(pivotcurrency, discountccy, spread)
			    curves(ccy).getZC(pivotrate, pivotZC)
			    }
    	}
	    
	    if (!repository.keySet.contains(key)) repository += (key -> scala.collection.mutable.Map(ccy -> newcurve))
	    else repository(key) += (ccy -> newcurve)
	    newcurve
	  }
	
	private def ratecurve(c:DiscountableCurve):RateCurve = c match { case r:RateCurve => r; case _ => throw new ClassCastException}
	
	/**
	 * Checks whether the given curve is already calculated and stored in the repository.
	 */
	def contains(ccy:Currency, discountccy:Currency, spread:Double) = {
		repository.keySet.contains(Pair(discountccy, spread)) && repository(Pair(discountccy, spread)).keySet.contains(ccy)
	 }
	
}

