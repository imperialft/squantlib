package squantlib.model.discountcurve

import squantlib.parameter.yieldparameter.YieldParameter
import squantlib.termstructures.ZCImpliedYieldTermStructure
import org.jquantlib.pricingengines.bond.DiscountingBondEngine
import org.jquantlib.instruments
import org.jquantlib.daycounters.{DayCounter, Thirty360}
import org.jquantlib.currencies.Currency
import org.jquantlib.time.{Calendar, Date => qlDate, Period => qlPeriod}

/**
 * Encapsulates cashflow discounting curve and discount spread on 3m float rate.
 * value = discount factor, ZCspread = discount spread on 3m float rate (not defined on FX discount)
 */

class DiscountCurve(val currency:Currency, val zc : YieldParameter, val discountspread : YieldParameter, val daycount:DayCounter, val fx:Double) extends YieldParameter {
  
	def this(c:Currency, v:YieldParameter, fx:Double) = this(c, v, null, new Thirty360, fx)
  
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
    val mindays : Long = zc.mindays
	/** 
	 * Returns number of days between value date and final defined point. 
	 * This point is the high boundary between interpolation & extrapolation.
	 */
    val maxdays : Long = zc.maxdays
	/**
	 * Returns the value corresponding to the given date.
	 * @param observation date as the number of calendar days after value date.
	 */
    def value(days : Long) : Double = zc(days)
    
    override def describe = "Currency:\t" + currency.code + sys.props("line.separator") + 
  				 "Spread:\t" + (if (discountspread == null) "N/A" else discountspread.describe) + 
  				 sys.props("line.separator") + 
  				 "ZC:\t" + zc.describe + sys.props("line.separator") 
}

