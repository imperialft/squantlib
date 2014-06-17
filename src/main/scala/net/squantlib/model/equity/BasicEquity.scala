package net.squantlib.model.equity

import net.squantlib.model.rates.DiscountCurve
import net.squantlib.model.yieldparameter.{YieldParameter, YieldParameter3D}
import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Period => qlPeriod}
import net.squantlib.util.Date
import scala.annotation.tailrec

/**
 * Orthodox index with continuous dividend yield.
 */
case class BasicEquity(
    val id:String,
	override var spot:Double,
    override val rateCurve:DiscountCurve, 
    override val dividendDates:Map[Date, Double], 
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
	  val divList = dividendList.filter(_._1 <= days).toList.sortBy(_._1)
	  val periods = if (!divList.isEmpty && divList.last._1 == days) divList else divList :+ (days, 0.0)
	  
	  @tailrec def fwdRec(s:Double, lastd:Double, dates:List[(Double, Double)]):Double = {
	    if (dates.isEmpty) s
	    else {
	      val (d, div) = dates.head
	      val newspot = s * math.exp((fwdDiscountRate(lastd, d) - fwdRepoRate(lastd, d)) * (d - lastd) / 365.25) - div
	      fwdRec(newspot, d, dates.tail)
	    }
	  }
	  
	  fwdRec(spot, 0.0, periods)
	}
	
    override def repoRate(days:Double):Double = repo(days)

    override def expectedYield:Option[Double] = 
      if (rateCurve == null) None else Some(rateCurve.impliedRate(365.0))
    
    override def expectedCoupon:Option[Double] = 
      if (dividendList.isEmpty) None
      else Some(dividendList.filter(_._1 <= 365).map(_._2).sum / spot)
      
} 

object BasicEquity {
  
  def apply(name:String, spot:Double, rateCurve:DiscountCurve, dividend:Map[Date, Double], repo:RepoCurve, vol:YieldParameter):BasicEquity = 
    BasicEquity(name, spot, rateCurve, dividend, repo, (y:Double, s:Double) => vol(y))
  
  def apply(name:String, spot:Double, rateCurve:DiscountCurve, dividend:Map[Date, Double], repo:RepoCurve, vol:YieldParameter3D):BasicEquity = 
    BasicEquity(name, spot, rateCurve, dividend, repo, (y:Double, s:Double) => vol(y, s))
    
}
