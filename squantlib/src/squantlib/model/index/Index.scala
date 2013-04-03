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
  
	val assetID = "INDEX"
  
	override val valuedate:qlDate
	
	val id:String
	
	val rateCurve:DiscountCurve
	
	val currency:Currency = rateCurve.currency
	
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
    def zcY(years:Double) = rateCurve(years * 365.25)
    
    def interestRate(days:Double) = rateCurve.impliedRate(days)
    def interestRate(date:qlDate) = rateCurve.impliedRate(date)
    def interestRate(period:qlPeriod) = rateCurve.impliedRate(period)
    def interestRate(dayfrac:Double, dayCounter:DayCounter) = rateCurve.impliedRate(dayfrac, dayCounter)
    def interestRateY(years:Double) = rateCurve.impliedRate(years * 365.25)
    
    def repoRate(days:Double):Double // To be implemented
    def repoRate(date:qlDate):Double = repoRate(toDays(date))
    def repoRate(period:qlPeriod):Double = repoRate(toDays(period))
    def repoRate(dayfrac:Double, dayCounter:DayCounter):Double = repoRate(toDays(dayfrac, dayCounter))
    def repoRateY(years:Double) = repoRate(years * 365.25)
     
    def dividendYield(days:Double):Double = interestRate(days) - repoRate(days) - math.log(forward(days) / spot) / (days / 365.25)
    def dividendYield(date:qlDate):Double = dividendYield(toDays(date))
    def dividendYield(period:qlPeriod):Double = dividendYield(toDays(period))
    def dividendYield(dayfrac:Double, dayCounter:DayCounter):Double = dividendYield(toDays(dayfrac, dayCounter))
    def dividendYieldY(years:Double) = dividendYield(years * 365.25)
    
    val maxDays = rateCurve.maxdays.min(rateCurve.maxdays)
} 
