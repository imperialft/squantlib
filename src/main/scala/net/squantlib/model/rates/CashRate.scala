package net.squantlib.model.rates

import net.squantlib.model.yieldparameter.YieldParameter
import org.jquantlib.daycounters.{DayCounter, Actual360}
import org.jquantlib.currencies.Currency
import org.jquantlib.indexes.IborIndex
import org.jquantlib.time.{Period => qlPeriod}
import net.squantlib.util.Date
import net.squantlib.model.yieldparameter._
import net.squantlib.model.rates.convention.RateConvention
import net.squantlib.model.asset.Underlying

/**
 * Cash rate as underlying
 */
case class CashRate(
    curve:DiscountCurve, 
    period:qlPeriod, 
    tenorbasis:Option[TenorBasisSwapCurve]) extends Underlying {
  
	val assetID = "CASH"
	
	val currency:Currency = curve.currency
	
	val valuedate = curve.valuedate
	
	val id = currency.code + period.length + period.units.getShortFormat
	
	val convention = RateConvention(currency.code)
	
	def tenorAdjust(days:Double):Double = {
	  val adj = tenorbasis.collect{case c => c.rate(days)}.getOrElse(0.0)
	  val defaultMonths:Int = convention.collect{case c => c.swapFloatIndex.tenor.length}.getOrElse(0)
	  if (defaultMonths <= 0 || adj.isNaN) {return 0.0}
	  val periodDays:Int = valuedate.days(period).toInt
	  
	  if (defaultMonths == 6) periodDays match {
	    case d if d >= 180 => 0.0
	    case d if d <= 90 => -adj
	    case d => -adj + adj * (d.toDouble - 90.0) / 180.0
	  }
	  
	  else periodDays match {
	    case d if d >= 180 => adj
	    case d if d <= 90 => 0.0
	    case d => adj * (d.toDouble - 90.0) / 180.0
	  }
	    
	}
	
    override def expectedYield:Option[Double] = Some(curve.impliedRate(valuedate.add(period)))
    
    override def expectedCoupon:Option[Double] = expectedYield
	
	/**
	 * Returns FX spot rate
	 */
	var spot:Double = curve.impliedRate(valuedate.add(period))
	
	override val latestPrice = Some(spot)
	
	/**
	 * Returns the volatility corresponding to the given date & strike.
	 * @param days observation date as the number of calendar days after value date.
	 * @param strike fx strike
	 */
	override def volatility(days:Double):Double = Double.NaN
	
	override def volatility(days:Double, strike:Double):Double = Double.NaN
	  
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    override def forward(days:Double):Double = {
	  val d = valuedate.add(days.round.toInt)
	  val dc = convention.collect{case c => c.iborindex(period).dayCounter}.getOrElse(new Actual360)
	  curve.forwardRate(d, d.add(period), dc) + tenorAdjust(days)
	}
	
    override val maxDays = curve.maxdays
    
    override def discountRate(days:Double):Double = curve.impliedRate(days)
    
    override def assetYield(days:Double):Double = 0.0

    override def repoRate(days:Double):Double = 0.0
  
    override val dividends:Map[Double, Double] = Map.empty
    
}


object CashRate {
  
}