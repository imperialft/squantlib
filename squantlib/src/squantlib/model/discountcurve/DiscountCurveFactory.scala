package squantlib.model.discountcurve

import squantlib.parameter.yieldparameter.{YieldParameter, FlatVector}
import org.jquantlib.currencies.Currency
import org.jquantlib.time.{Date => JDate, Period => JPeriod, TimeUnit}
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions

/** 
 * Stores rate curve information and initialize discount curves as requested.
 * Require all discount curves to have same value date.
 */
class DiscountCurveFactory(val curves:Map[String, DiscountableCurve]) {

	val valuedate = curves.head._2.valuedate
	require(curves.forall(c => c._2.valuedate == valuedate))
	
	
	/** 
	 * USD
	 */
	val pivotcurrency = BasisSwapCurve.pivotcurrency.code

	/** 
	 * Discounting Curves
	 */
	val discountingcurves = { curves.map{ case (cur, curve) => (cur, curve match { case r:RateCurve => r; case _ => null})}}.filter{case(k, c) => c != null}
	
	/**
	 * Stores already calculated discount curves.
	 * Assumption: for each key, value contains discount curve for both discount and pivot currency.
	 */
	var repository:Map[(String, Double), scala.collection.mutable.Map[String, DiscountCurve]] = Map.empty

	/**
	 * Returns discount curve. Discount currency is the same currency with given spread.
	 */
	def getcurve(ccy:String, spread:Double):DiscountCurve = getcurve(ccy, ccy, spread)
	
	/**
	 * Returns discount curve. Discount currency is as specified with zero spread.
	 */
	def getcurve(ccy:String, discountccy:String):DiscountCurve = getcurve(ccy, discountccy, 0.0)

	/**
	 * Returns discount curve. Discount currency is as specified with given spread.
	 */
	def getcurve(ccy:String, discountccy:String, spread:Double) : DiscountCurve = 
	  if (contains(ccy, discountccy, spread)) repository(Pair(discountccy, spread))(ccy)
	  else {
	    val key = (discountccy, spread)
	    val newcurve = ccy match {
		    case `discountccy` => { 
		      val rate = ratecurve(ccy)
		      val flatvector = new FlatVector(valuedate, Map(new JPeriod(6, TimeUnit.Months) -> spread))
		      val zccurve = curves(ccy).getZC(flatvector)
		      zccurve
		      }
		    					
		    case `pivotcurrency` => { 
		        val discountrate = ratecurve(discountccy)
			    val zccurve = getcurve(discountccy, discountccy, spread)
			    curves(ccy).getZC(discountrate, zccurve)
			    }
		      
		    case _ => { 
		        val pivotrate = ratecurve(pivotcurrency)
			    val pivotZC = getcurve(pivotcurrency, discountccy, spread)
			    curves(ccy).getZC(pivotrate, pivotZC)
			    }
    	}
	    
	    if (!repository.keySet.contains(key)) repository += (key -> scala.collection.mutable.Map(ccy -> newcurve))
	    else repository(key) += (ccy -> newcurve)
	    newcurve
	  }
	
	private def ratecurve(c:String):RateCurve = if (discountingcurves.keySet.contains(c)) discountingcurves(c) else throw new ClassCastException
	
	/**
	 * Checks whether the given curve is already calculated and stored in the repository.
	 */
	def contains(ccy:String, discountccy:String, spread:Double) = {
		repository.keySet.contains(Pair(discountccy, spread)) && repository(Pair(discountccy, spread)).keySet.contains(ccy)
	 }
	
	def describe:Unit = {println("DiscountingCurves:")
						discountingcurves.foreach(c => c._2.describe)
						println("Curves:")
						curves.filter(c => !discountingcurves.keySet.contains(c._1)).foreach(c => c._2.describe)
	}
	
    override def toString():String = "DiscountCurveFactory{" + curves.map(c => c._2).mkString(", ") + "}"
	
}

