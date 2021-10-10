package net.squantlib.model.index

import net.squantlib.model.asset.Underlying
import net.squantlib.model.yieldparameter.YieldParameter
import net.squantlib.model.rates.DiscountCurve
import net.squantlib.util.Date
import net.squantlib.util.ql.currencies.Currency
import net.squantlib.util.ql.daycounters.DayCounter
import net.squantlib.util.ql.time.{Period => qlPeriod}

/**
 * Basic Index framework providing spot, forward and volatility
 * 
 * @constructor stores each information
 * @param float index, daycount & payment frequency for fixed leg
 */
trait Index extends Underlying {
  
	val assetID = "INDEX"
  
	override val valuedate:Date
	
	val id:String
	
	val rateCurve:DiscountCurve
	
	override val currency:Currency = rateCurve.currency
	
	val discontinousDates:Set[Date]
	
	/**
	 * Returns spot price
	 */
	var spot:Double
	
//	/**
//	 * Returns the volatility corresponding to the given date & strike.
//	 * @param days observation date as the number of calendar days after value date.
//	 * @param strike strike
//	 */
//	override def volatility(days:Double):Double
//	override def volatility(days:Double, strike:Double):Double
	  
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    override def forward(days : Double) : Double
    
    def zc(days:Double) = rateCurve(days)
    def zc(date:Date) = rateCurve(date)
    def zc(period:qlPeriod) = rateCurve(period)
    def zc(dayfrac:Double, dayCounter:DayCounter) = rateCurve(dayfrac, dayCounter)
    def zcY(years:Double) = rateCurve(years * 365.25)
    
    override def discountRate(days:Double) = rateCurve.impliedRate(days)
    
    override def repoRate(days:Double):Double // To be implemented
     
    def dividendYield(days:Double):Double = discountRate(days) - repoRate(days) - math.log(forward(days) / spot) / (days / 365.25)
    def dividendYield(date:Date):Double = dividendYield(toDays(date))
    def dividendYield(period:qlPeriod):Double = dividendYield(toDays(period))
    def dividendYield(dayfrac:Double, dayCounter:DayCounter):Double = dividendYield(toDays(dayfrac, dayCounter))
    def dividendYieldY(years:Double) = dividendYield(years * 365.25)
    
    val maxDays = rateCurve.maxdays.min(rateCurve.maxdays)
} 
