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
	var repository:HashMap[(Currency, Double), HashMap[Currency, DiscountCurve]] = HashMap.empty

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
	    println("not contained")
	    val key = (discountccy, spread)
	    val newcurve = ccy match {
		    case `discountccy` => { 
		      println(ccy.name + " is discount currency")
		      val rate = ratecurve(curves(ccy))
		      val flatvector = new FlatVector(valuedate, Map(new JPeriod(6, TimeUnit.Months) -> spread))
		      val zccurve = curves(ccy).getZC(flatvector)
		      println(ccy.name + " discount ccy initialised vd "+ zccurve.valuedate + " 10yrzc:" + zccurve.zc.value(new JPeriod(10, TimeUnit.Months)))
		      zccurve
		      }
		    					
		    case `pivotcurrency` => { 
		      println(ccy.name + " is pivot currency")
		        val discountrate = ratecurve(curves(discountccy))
			    val zccurve = discountcurve(discountccy, discountccy, spread)
			    curves(ccy).getZC(discountrate, zccurve)
			    }
		      
		    case _ => { 
		    	println(ccy.name + " is other currency")
		        val pivotrate = ratecurve(curves(pivotcurrency))
			    val pivotZC = discountcurve(pivotcurrency, discountccy, spread)
			    curves(ccy).getZC(pivotrate, pivotZC)
			    }
    	}
	    
	    println("add to repository")
	    if (!repository.keySet.contains(key)) { println ("adding new content"); repository += (key -> HashMap(ccy -> newcurve))}
	    else { println ("adding new index"); repository(key) += (ccy -> newcurve)}
	    
	    println("added")
	    newcurve
	  }
	
	private def ratecurve(c:DiscountableCurve):RateCurve = c match { case r:RateCurve => r; case _ => throw new ClassCastException}
	
	/**
	 * Checks whether the given curve is already calculated and stored in the repository.
	 */
	def contains(ccy:Currency, discountccy:Currency, spread:Double) = {
		println("check content " + ccy.name + ", " + discountccy.name + ", " + spread)
		val key = (discountccy, spread)
		repository.keySet.contains(key) && repository(key).keySet.contains(ccy)
	 }
	
}

