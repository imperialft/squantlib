package net.squantlib.model.rates

import net.squantlib.model.yieldparameter.{YieldParameter, YieldParameter3D}
import net.squantlib.schedule.Schedule
import net.squantlib.util.Date
import net.squantlib.util.initializer.Calendars
import net.squantlib.model.rates.convention.RateConvention
import org.jquantlib.pricingengines.bond.DiscountingBondEngine
import org.jquantlib.instruments
import org.jquantlib.daycounters.{DayCounter, Thirty360, Actual365Fixed}
import org.jquantlib.currencies.Currency
import org.jquantlib.time.{Calendar, Period => qlPeriod, Frequency, BusinessDayConvention}
import java.lang.UnsupportedOperationException

/**
 * Encapsulates cashflow discounting curve and discount spread on 3m float rate.
 * value = discount factor, ZCspread = discount spread on 3m float rate (not defined on FX discount)
 */

case class DiscountCurve(
    currency:Currency, 
    zc:YieldParameter, 
    discountspread:YieldParameter, 
    fx:Double, 
    vol:Option[RateVolatility]) extends YieldParameter {
  
	private var vd = zc.valuedate
  
	/**
	 * Returns base date of this vector. 
	 */
	def valuedate = vd
	def valuedate_= (d:Date) = {
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
    
    def impliedRate(days:Double):Double = -math.log(value(days)) / (math.max(days, 1.0) / 365.25)
    def impliedRate(days:Long):Double = -math.log(value(days.toDouble)) / (math.max(days, 1.0) / 365.25)
    def impliedRate(dayfrac:Double, dayCounter:DayCounter):Double = impliedRate((dayfrac * 365.25 / dayCounter.annualDayCount))
    def impliedRate(date:Date):Double = impliedRate(date.serialNumber - valuedate.serialNumber)
    def impliedRate(period:qlPeriod):Double = impliedRate(valuedate.days(period))
    
    def impliedLibor(days:Double, dayCounter:DayCounter):Double = (1.0 / value(days) - 1.0) / (days / dayCounter.annualDayCount)
    def impliedLibor(days:Long, dayCounter:DayCounter):Double = impliedLibor(days, dayCounter)
    def impliedLibor(date:Date, dayCounter:DayCounter):Double = impliedLibor(date.serialNumber - valuedate.serialNumber, dayCounter)
    def impliedLibor(period:qlPeriod, dayCounter:DayCounter):Double = impliedLibor(valuedate.days(period), dayCounter)
    
    def forwardRate(expiry:Double, maturity:Double):Double = (impliedRate(expiry + maturity) * (expiry + maturity) - impliedRate(expiry) * (expiry)) / maturity
    def forwardRate(expiry:Long, maturity:Long):Double = forwardRate(expiry.toDouble, maturity.toDouble)
    def forwardRate(expiry:Date, maturity:Date):Double = forwardRate(expiry.serialNumber - valuedate.serialNumber, maturity.serialNumber - expiry.serialNumber)
    def forwardRate(expiry:qlPeriod, maturity:qlPeriod):Double = forwardRate(valuedate.days(expiry), valuedate.days(maturity) - valuedate.days(expiry))
    
    def forwardRate(expiry:Double, maturity:Double, dayCounter:DayCounter):Double = (math.exp(forwardRate(expiry, maturity) * maturity / 365.0) - 1.0) / (maturity / 365.0 * dayCounter.annualDayCount)
    def forwardRate(expiry:Long, maturity:Long, dayCounter:DayCounter):Double = forwardRate(expiry.toDouble, maturity.toDouble, dayCounter)
    def forwardRate(expiry:Date, maturity:Date, dayCounter:DayCounter):Double = forwardRate(expiry.serialNumber - valuedate.serialNumber, maturity.serialNumber - expiry.serialNumber, dayCounter)
    def forwardRate(expiry:qlPeriod, maturity:qlPeriod, dayCounter:DayCounter):Double = forwardRate(valuedate.days(expiry), valuedate.days(maturity) - valuedate.days(expiry), dayCounter)
    
    def volatility(expiry:Double, maturity:Double):Option[Double] = vol.collect{case v => v(expiry, maturity)}
    def volatility(expiry:Long, maturity:Long):Option[Double] = vol.collect{case v => v(expiry, maturity)}
    def volatility(expiry:Date, maturity:Date):Option[Double] = vol.collect{case v => v(expiry, maturity)}
    def volatility(expiry:qlPeriod, maturity:qlPeriod):Option[Double] = vol.collect{case v => v(expiry, maturity)}
    
    def duration(maturity:Date):Double = RateConvention(currency.code) match {
      case Some(conv) => duration(maturity, if (conv.swapFixPeriod == null) new qlPeriod("3M") else conv.swapFixPeriod.toPeriod, if (conv.swapFixDaycount == null) new Actual365Fixed else conv.swapFixDaycount, BusinessDayConvention.ModifiedFollowing)
      case None => duration(maturity, new qlPeriod("3M"), new Actual365Fixed, BusinessDayConvention.ModifiedFollowing)
    }
    
    def duration(maturity:qlPeriod, period:qlPeriod, daycount:DayCounter, convention:BusinessDayConvention):Double = 
      duration(valuedate.add(maturity), period, daycount, convention)
    
    def duration(maturity:Date, period:qlPeriod, daycount:DayCounter, convention:BusinessDayConvention):Double = {
      val dates = Schedule.periodicalDates(valuedate, maturity, period, convention, Calendars.empty)
      (for (i <- 1 to dates.size - 1) yield (Date.daycount(dates(i-1), dates(i), daycount) * value(dates(i)))).sum
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