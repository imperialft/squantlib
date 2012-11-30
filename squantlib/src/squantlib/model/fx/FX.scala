package squantlib.model.fx

import squantlib.model.Underlying
import squantlib.model.rates.DiscountCurve
import squantlib.model.yieldparameter.YieldParameter
import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod}

/**
 * Basic FX framework providing spot, forward and volatility
 * 
 * @constructor stores each information
 * @param float index, daycount & payment frequency for fixed leg
 */
trait FX extends Underlying {
  
	val curveDom:DiscountCurve
	val curveFor:DiscountCurve 
	
	val currencyDom:Currency = curveDom.currency
	val currencyFor:Currency = curveFor.currency
	
	require (curveDom.valuedate eq curveFor.valuedate)
	val valuedate = curveDom.valuedate
	
	val name = currencyFor.code + currencyDom.code
	
	/**
	 * Returns FX spot rate
	 */
	var spot:Double = curveDom.fx / curveFor.fx
	
	/**
	 * Returns the volatility corresponding to the given date & strike.
	 * @param days observation date as the number of calendar days after value date.
	 * @param strike fx strike
	 */
	def volatility(days:Double, strike:Double):Double
	  
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    override def forward(days : Double) : Double = spot * curveFor(days) / curveDom(days)
    
    
    def zcDom(days:Double) = curveDom(days)
    def zcDom(date:qlDate) = curveDom(date)
    def zcDom(period:qlPeriod) = curveDom(period)
    def zcDom(dayfrac:Double, dayCounter:DayCounter) = curveDom(dayfrac, dayCounter)
    
    def zcFor(days:Double) = curveFor(days)
    def zcFor(date:qlDate) = curveFor(date)
    def zcFor(period:qlPeriod) = curveFor(period)
    def zcFor(dayfrac:Double, dayCounter:DayCounter) = curveFor(dayfrac, dayCounter)
    
    def rateDom(days:Double) = curveDom.impliedRate(days)
    def rateDom(date:qlDate) = curveDom.impliedRate(date)
    def rateDom(period:qlPeriod) = curveDom.impliedRate(period)
    def rateDom(dayfrac:Double, dayCounter:DayCounter) = curveDom.impliedRate(dayfrac, dayCounter)
    
    def rateFor(days:Double) = curveFor.impliedRate(days)
    def rateFor(date:qlDate) = curveFor.impliedRate(date)
    def rateFor(period:qlPeriod) = curveFor.impliedRate(period)
    def rateFor(dayfrac:Double, dayCounter:DayCounter) = curveFor.impliedRate(dayfrac, dayCounter)
    
    val maxDays = curveDom.maxdays.min(curveFor.maxdays)
} 
