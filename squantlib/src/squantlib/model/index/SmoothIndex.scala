package squantlib.model.index

import squantlib.model.rates.DiscountCurve
import squantlib.model.yieldparameter.{YieldParameter, YieldParameter3D}
import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod}

/**
 * Orthodox index with continuous dividend yield.
 */
case class SmoothIndex(
    id:String, 
	var spot:Double,
    rateCurve:DiscountCurve, 
    dividend:DividendCurve, 
    repo:RepoCurve, 
    vol:(Double, Double) => Double) extends Index {
  
	override val valuedate = rateCurve.valuedate
	
	override val discontinousDates = Set.empty[qlDate]
	
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
    override def forward(days : Double) : Double = spot * math.exp((rateCurve.impliedRate(days) - repo(days) - dividend(days)) * days / 365.25)

    override def repoRate(days:Double):Double = repo(days)
    
    override def dividendYield(days:Double):Double = dividend(days)
    
} 

object SmoothIndex {
  
  def apply(name:String, spot:Double, rateCurve:DiscountCurve, dividend:DividendCurve, repo:RepoCurve, vol:YieldParameter):SmoothIndex = 
    SmoothIndex(name, spot, rateCurve, dividend, repo, (y:Double, s:Double) => vol(y))
  
  def apply(name:String, spot:Double, rateCurve:DiscountCurve, dividend:DividendCurve, repo:RepoCurve, vol:YieldParameter3D):SmoothIndex = 
    SmoothIndex(name, spot, rateCurve, dividend, repo, (y:Double, s:Double) => vol(y, s))
    
}
