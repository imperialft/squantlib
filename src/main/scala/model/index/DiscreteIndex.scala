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
    id:String,
	var spot:Double,
    rateCurve:DiscountCurve, 
    dividend:Map[qlDate, Double], 
    repo:RepoCurve, 
    vol:(Double, Double) => Double
    ) extends Index {
  
	override val valuedate = rateCurve.valuedate
	
	override val discontinousDates = dividend.keySet
	
	override val latestPrice = Some(spot)
	
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
	
    override def repoRate(days:Double):Double = repo(days)
    
    val dividendMap:Map[Double, Double] = dividend.map{case (d, v) => (toDays(d), v)}
    
    val dividendMapY:Map[Double, Double] = dividend.map{case (d, v) => (toDays(d)/365.25, v)}

    override def expectedYield:Option[Double] = {
	  // TO BE IMPLEMENTED
      None
    }
    
    override def expectedCoupon:Option[Double] = {
	  // TO BE IMPLEMENTED
      expectedYield    
    }
} 
