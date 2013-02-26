package squantlib.model

import squantlib.model.rates.DiscountCurve
import org.jquantlib.currencies.Currency
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod}

/**
 * Underlying to be used for pricing models.
 */
trait Underlying {
  
	val valuedate:qlDate
	val name:String
	var spot:Double  // TO BE DEFINED IN SUBCLASS
	
	/**
	 * Returns the atm volatility corresponding to the given date.
	 * @param days observation date as the number of calendar days after value date.
	 */
	def volatility(days:Double):Double // TO BE DEFINED IN SUBCLASS
	
	/**
	 * Returns the volatility corresponding to the given date & strike.
	 * @param days observation date as the number of calendar days after value date.
	 * @param strike 
	 */
	def volatility(days:Double, strike:Double):Double // TO BE DEFINED IN SUBCLASS
	
	/**
	 * Returns the volatility corresponding to the given date & strike.
	 * @param observation date as day count fraction and its day count method.
	 * @param strike
	 */
	def volatility(dayfrac:Double, dayCounter:DayCounter):Double = volatility(toDays(dayfrac, dayCounter))	
	def volatility(dayfrac:Double, dayCounter:DayCounter, strike:Double):Double = volatility(toDays(dayfrac, dayCounter), strike)	
	
	/**
	 * Returns the volatility corresponding to the given date & strike.
	 * @param observation date
	 * @param strike
	 */
	def volatility(date:qlDate):Double = volatility(toDays(date))	
	def volatility(date:qlDate, strike:Double):Double = volatility(toDays(date), strike)	
	
	/**
	 * Returns the volatility corresponding to the given date & strike.
	 * @param observation date
	 * @param observation date as the period from value date.
	 */
	def volatility(period:qlPeriod):Double = volatility(toDays(period))	
	def volatility(period:qlPeriod, strike:Double):Double = volatility(toDays(period), strike)	
	
	/**
	 * Returns the volatility corresponding to the given date represented in nb of years & strike.
	 * @param observation date
	 * @param observation date as the period from value date.
	 */
	def volatilityY(years:Double):Double = volatility(years * 365.25)
	def volatilityY(years:Double, strike:Double):Double = volatility(years * 365.25, strike)
	  
	/**
	 * Returns the forward price corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    def forward(days:Double):Double // TO BE DEFINED IN SUBCLASS
    
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as day count fraction and its day count method.
	 */
    def forward(dayfrac:Double, dayCounter:DayCounter):Double = forward(toDays(dayfrac, dayCounter))
    
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date
	 */
    def forward(date:qlDate):Double = forward(toDays(date))
    
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the period from value date.
	 */
    def forward(period:qlPeriod):Double = forward(toDays(period))
    
	/**
	 * Returns the latest defined date.
	 */
    def maxDays:Double
    
	
	/**
	 * Private date conversion functions
	 */
    protected def toDays(dayfrac:Double, dayCounter:DayCounter) = (dayfrac * 365.25 / dayCounter.annualDayCount)
    protected def toDays(date:qlDate) = (date.serialNumber() - valuedate.serialNumber()).toDouble
    protected def toDays(period:qlPeriod) = period.days(valuedate).toDouble
    
} 