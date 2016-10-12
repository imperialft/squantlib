package net.squantlib.model.fx

import net.squantlib.model.asset.Underlying
import net.squantlib.model.rates.DiscountCurve
import net.squantlib.util.Date
import net.squantlib.util.initializer.Calendars
import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Period => qlPeriod}

/**
 * Basic FX framework providing spot, forward and volatility
 */
trait FX extends Underlying {
  
	val assetID = "FX"
	
	val curveDom:DiscountCurve
	val curveFor:DiscountCurve 
	
	val currencyDom:Currency = curveDom.currency
	val currencyFor:Currency = curveFor.currency
	override val currency = currencyDom
	
	override lazy val fixingCalendar = Calendars.empty
	
  override lazy val paymentCalendar = Calendars.empty
	
	require (curveDom.valuedate eq curveFor.valuedate)
	val valuedate = curveDom.valuedate
	
	val id = currencyFor.code + currencyDom.code
	
    override def expectedYield:Option[Double] = Some(curveFor.impliedRate(360) - curveDom.impliedRate(360))
    
    override def expectedCoupon:Option[Double] = expectedYield
    
    override def repoRate(days:Double) = 0.0
	
    override val dividends:Map[Double, Double] = Map.empty

	/**
	 * Returns FX spot rate
	 */
	var spot:Double = curveDom.fx / curveFor.fx
	
	override val latestPrice = Some(spot)
	
	/**
	 * Returns the volatility corresponding to the given date & strike.
	 * @param days observation date as the number of calendar days after value date.
	 * @param strike fx strike
	 */
	override def volatility(days:Double):Double
	override def volatility(days:Double, strike:Double):Double
	  
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    override def forward(days : Double) : Double = spot * curveFor(days) / curveDom(days)
    
    def zcDom(days:Double) = curveDom(days)
    def zcDom(date:Date) = curveDom(date)
    def zcDom(period:qlPeriod) = curveDom(period)
    def zcDom(dayfrac:Double, dayCounter:DayCounter) = curveDom(dayfrac, dayCounter)
    def zcDomY(years:Double) = curveDom(years * 365.25)
    
    def zcFor(days:Double) = curveFor(days)
    def zcFor(date:Date) = curveFor(date)
    def zcFor(period:qlPeriod) = curveFor(period)
    def zcFor(dayfrac:Double, dayCounter:DayCounter) = curveFor(dayfrac, dayCounter)
    def zcForY(years:Double) = curveFor(years * 365)
    
    def rateDom(days:Double) = curveDom.impliedRate(days)
    def rateDom(date:Date) = curveDom.impliedRate(date)
    def rateDom(period:qlPeriod) = curveDom.impliedRate(period)
    def rateDom(dayfrac:Double, dayCounter:DayCounter) = curveDom.impliedRate(dayfrac, dayCounter)
    def rateDomY(years:Double) = curveDom.impliedRate(years * 365)
    
    def rateFor(days:Double) = curveFor.impliedRate(days)
    def rateFor(date:Date) = curveFor.impliedRate(date)
    def rateFor(period:qlPeriod) = curveFor.impliedRate(period)
    def rateFor(dayfrac:Double, dayCounter:DayCounter) = curveFor.impliedRate(dayfrac, dayCounter)
    def rateForY(years:Double) = curveFor.impliedRate(years * 365)
    
    override def discountRate(days:Double) = rateDom(days)
    override def assetYield(days:Double) = rateFor(days)
    
    override val maxDays = curveDom.maxdays.min(curveFor.maxdays)
    
} 
