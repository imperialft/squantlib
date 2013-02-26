package squantlib.model.index

import squantlib.model.Underlying
import squantlib.model.yieldparameter.YieldParameter
import squantlib.model.rates.DiscountCurve
import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod}

/**
 * Basic Index framework providing spot, forward and volatility
 * 
 * @constructor stores each information
 * @param float index, daycount & payment frequency for fixed leg
 */
trait Index extends Underlying {
  
	override val valuedate:qlDate
	
	val rateCurve:DiscountCurve
	
	val name:String
	
	val discontinousDates:Set[qlDate]
	
	/**
	 * Returns spot price
	 */
	var spot:Double
	
	/**
	 * Returns the volatility corresponding to the given date & strike.
	 * @param days observation date as the number of calendar days after value date.
	 * @param strike strike
	 */
	override def volatility(days:Double):Double
	override def volatility(days:Double, strike:Double):Double
	  
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    override def forward(days : Double) : Double
    
    def zc(days:Double) = rateCurve(days)
    def zc(date:qlDate) = rateCurve(date)
    def zc(period:qlPeriod) = rateCurve(period)
    def zc(dayfrac:Double, dayCounter:DayCounter) = rateCurve(dayfrac, dayCounter)
    def zcY(years:Double) = rateCurve(years)
    
    def interestRate(days:Double) = rateCurve.impliedRate(days)
    def interestRate(date:qlDate) = rateCurve.impliedRate(date)
    def interestRate(period:qlPeriod) = rateCurve.impliedRate(period)
    def interestRate(dayfrac:Double, dayCounter:DayCounter) = rateCurve.impliedRate(dayfrac, dayCounter)
    def interestRateY(years:Double) = rateCurve.impliedRate(years * 365)
    
    val maxDays = rateCurve.maxdays.min(rateCurve.maxdays)
} 
