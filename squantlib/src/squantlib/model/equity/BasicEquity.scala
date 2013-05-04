package squantlib.model.equity

import squantlib.model.rates.DiscountCurve
import squantlib.model.yieldparameter.{YieldParameter, YieldParameter3D}
import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod}

/**
 * Orthodox index with continuous dividend yield.
 */
case class BasicEquity(
    override val id:String,
	override var spot:Double,
    override val rateCurve:DiscountCurve, 
    override val dividends:Map[qlDate, Double], 
    repo:RepoCurve, 
    vol:(Double, Double) => Double
    ) extends Equity {
  
	override val valuedate = rateCurve.valuedate
	
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
    override def forward(days:Double):Double = {
	  val divList = dividendDaysList.filter(d => d._1 > 0 && d._1 <= days).toList.sortBy(_._1)
	  val periods = if (!divList.isEmpty && divList.last._1 == days) divList else divList :+ (days, 0.0)
	  var s = spot
	  var lastd = 0.0
	  
	  periods.foreach{case (d, div) =>
	    val dt = (d - lastd) / 365.25
	    s = s * math.exp((fwdInterestRate(lastd, d) - fwdRepoRate(lastd, d)) * dt) - div
	    lastd = d
	  }
	  
	  s
	}
	
    override def repoRate(days:Double):Double = repo(days)
    

    override def expectedYield:Option[Double] = {
	  // TO BE IMPLEMENTED
      None
    }
    
    override def expectedCoupon:Option[Double] = {
	  // TO BE IMPLEMENTED
      expectedYield    
    }
} 

object BasicEquity {
  
  def apply(name:String, spot:Double, rateCurve:DiscountCurve, dividend:Map[qlDate, Double], repo:RepoCurve, vol:YieldParameter):BasicEquity = 
    BasicEquity(name, spot, rateCurve, dividend, repo, (y:Double, s:Double) => vol(y))
  
  def apply(name:String, spot:Double, rateCurve:DiscountCurve, dividend:Map[qlDate, Double], repo:RepoCurve, vol:YieldParameter3D):BasicEquity = 
    BasicEquity(name, spot, rateCurve, dividend, repo, (y:Double, s:Double) => vol(y, s))
    
}
