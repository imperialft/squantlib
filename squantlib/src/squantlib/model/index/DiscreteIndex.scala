package squantlib.model.index

import squantlib.model.rates.DiscountCurve
import squantlib.model.yieldparameter.YieldParameter
import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod}

/**
 * Orthodox index with continuous dividend yield.
 */
case class DiscreteIndex(
	var spot:Double,
    rateCurve:DiscountCurve, 
    dividend:Map[qlDate, Double], 
    repoRate:RepoCurve, 
    vol:(Double, Double) => Double,
    name:String) extends Index {
  
	override val valuedate = rateCurve.valuedate
	
	override val discontinousDates = dividend.keySet
	
	/**
	 * Returns the volatility corresponding to the given date & strike.
	 * @param days observation date as the number of calendar days after value date.
	 * @param strike index strike
	 */
	override def volatility(days:Double):Double = vol(days, spot)
	override def volatility(days:Double, strike:Double):Double = vol(days, strike)
	
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    override def forward(days : Double):Double = {
	  // TO BE IMPLEMENTED
	  Double.NaN
	}
    
} 
