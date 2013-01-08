package squantlib.model.rates

import squantlib.model.yieldparameter.{YieldParameter, YieldParameter3D}
import squantlib.jquantlib.termstructures.ZCImpliedYieldTermStructure
import squantlib.util.DateUtils
import org.jquantlib.pricingengines.bond.DiscountingBondEngine
import org.jquantlib.instruments
import org.jquantlib.daycounters.{DayCounter, Thirty360, Actual365Fixed}
import org.jquantlib.currencies.Currency
import org.jquantlib.time.{Calendar, Date => qlDate, Period => qlPeriod, Frequency, BusinessDayConvention}
import org.jquantlib.time.calendars.NullCalendar
import java.lang.UnsupportedOperationException

/**
 * Encapsulates cashflow discounting curve and discount spread on 3m float rate.
 * value = discount factor, ZCspread = discount spread on 3m float rate (not defined on FX discount)
 */

case class DiscountCurve(currency:Currency, zc:YieldParameter, discountspread:YieldParameter, fx:Double, vol:Option[RateVolatility]) extends YieldParameter {
  
  	def toZCImpliedYieldTermStructure = new ZCImpliedYieldTermStructure(this)
  	def toZCImpliedYieldTermStructure(calendar:Calendar) = new ZCImpliedYieldTermStructure(this, calendar)
  
  	def toDiscountBondEngine = new DiscountingBondEngine(toZCImpliedYieldTermStructure)
  	def toDiscountBondEngine(calendar:Calendar) = new DiscountingBondEngine(toZCImpliedYieldTermStructure(calendar))
	
	private var vd = zc.valuedate
  
	/**
	 * Returns base date of this vector. 
	 */
	def valuedate = vd
	def valuedate_= (d:qlDate) = {
	  zc.valuedate = d
	  if (discountspread != null) discountspread.valuedate = d
	  vd = d
	}
	
	/**
	 * Returns number of days between value date and first defined point.
	 * This point is the low boundary between interpolation & extrapolation.
	 */
    val mindays : Double = zc.mindays
	/** 
	 * Returns number of days between value date and final defined point. 
	 * This point is the high boundary between interpolation & extrapolation.
	 */
    val maxdays : Double = zc.maxdays
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    def value(days : Double) : Double = zc(days)
    
    def impliedRate(days:Double):Double = -Math.log(value(days)) / (days / 365.25)
    def impliedRate(days:Long):Double = -Math.log(value(days.toDouble)) / (days / 365.25)
    def impliedRate(dayfrac:Double, dayCounter:DayCounter):Double = impliedRate((dayfrac * 365.25 / dayCounter.annualDayCount))
    def impliedRate(date:qlDate):Double = impliedRate(date.serialNumber - valuedate.serialNumber)
    def impliedRate(period:qlPeriod):Double = impliedRate(period.days(valuedate))
    
    def forwardRate(expiry:Double, maturity:Double):Double = (impliedRate(expiry + maturity) * (expiry + maturity) - impliedRate(expiry) * (expiry)) / maturity
    def forwardRate(expiry:Long, maturity:Long):Double = forwardRate(expiry.toDouble, maturity.toDouble)
    def forwardRate(expiry:qlDate, maturity:qlDate):Double = forwardRate(expiry.serialNumber - valuedate.serialNumber, maturity.serialNumber - expiry.serialNumber)
    def forwardRate(expiry:qlPeriod, maturity:qlPeriod):Double = forwardRate(expiry.days(valuedate), maturity.days(valuedate) - expiry.days(valuedate))
    
    def volatility(expiry:Double, maturity:Double):Option[Double] = vol.collect{case v => v(expiry, maturity)}
    def volatility(expiry:Long, maturity:Long):Option[Double] = vol.collect{case v => v(expiry, maturity)}
    def volatility(expiry:qlDate, maturity:qlDate):Option[Double] = vol.collect{case v => v(expiry, maturity)}
    def volatility(expiry:qlPeriod, maturity:qlPeriod):Option[Double] = vol.collect{case v => v(expiry, maturity)}
    
    def duration(maturity:qlPeriod, period:qlPeriod, daycount:DayCounter, convention:BusinessDayConvention):Double = 
      duration(valuedate.add(maturity), period, daycount, convention)
    
    def duration(maturity:qlDate, period:qlPeriod, daycount:DayCounter, convention:BusinessDayConvention):Double = {
      val dates = DateUtils.periodicalDates(valuedate, maturity, period, convention, new NullCalendar)
      (for (i <- 1 to dates.size - 1) yield (daycount.yearFraction(dates(i-1), dates(i)) * value(dates(i)))).sum
    }
    
    def shifted(shift:(Double, Double) => Double):DiscountCurve = throw new UnsupportedOperationException("shift on discount curve prohibited - please shift corresponding interest rate instead")
    
    override def describe = "Currency:\t" + currency.code + sys.props("line.separator") + 
  				 "Spread:\t" + (if (discountspread == null) "N/A" else discountspread.describe) + 
  				 sys.props("line.separator") + 
  				 "ZC:\t" + zc.describe + sys.props("line.separator") 
}

object DiscountCurve {
  
  def apply(c:Currency, v:YieldParameter, fx:Double):DiscountCurve = DiscountCurve(c, v, null, fx, None)
  
}