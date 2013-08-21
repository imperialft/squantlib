package squantlib.model.equity

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
trait Equity extends Underlying {
  
	val assetID = "EQUITY"
  
	override val valuedate:qlDate
	
	val id:String
	
	val rateCurve:DiscountCurve
	
	override val currency:Currency = rateCurve.currency
	
	val dividendDates:Map[qlDate, Double] // TO BE DEFINED IN SUBCLASS
	
    override lazy val dividends:Map[Double, Double] = dividendDates.filter{case (d, _) => d ge valuedate}.map{case (d, v) => (toDays(d), v)}
    
    override def assetYield(days:Double):Double = 0.0
    
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
    override def forward(days:Double) : Double
    
    def zc(days:Double) = rateCurve(days)
    def zc(date:qlDate) = rateCurve(date)
    def zc(period:qlPeriod) = rateCurve(period)
    def zc(dayfrac:Double, dayCounter:DayCounter) = rateCurve(dayfrac, dayCounter)
    def zcY(years:Double) = rateCurve(years * 365.25)
    
    def discountRate(days:Double) = rateCurve.impliedRate(days)
    
    def repoRate(days:Double):Double // To be implemented
     
    def dividendYield(days:Double):Double = discountRate(days) - repoRate(days) - math.log(forward(days) / spot) / (days / 365.25)
    def dividendYield(date:qlDate):Double = dividendYield(toDays(date))
    def dividendYield(period:qlPeriod):Double = dividendYield(toDays(period))
    def dividendYield(dayfrac:Double, dayCounter:DayCounter):Double = dividendYield(toDays(dayfrac, dayCounter))
    def dividendYieldY(years:Double) = dividendYield(years * 365.25)
    
    val maxDays = rateCurve.maxdays.min(rateCurve.maxdays)
} 
