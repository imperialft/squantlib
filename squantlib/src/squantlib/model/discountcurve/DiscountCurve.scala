package squantlib.model.discountcurve

import squantlib.parameter.yieldparameter.YieldParameter
import squantlib.termstructures.ZCImpliedYieldTermStructure
import org.jquantlib.pricingengines.bond.DiscountingBondEngine
import org.jquantlib.instruments
import org.jquantlib.daycounters.{DayCounter, Thirty360}
import org.jquantlib.currencies.Currency
import org.jquantlib.time.Calendar

/**
 * Encapsulates cashflow discounting curve and discount spread on 3m float rate.
 * value = discount factor, ZCspread = discount spread on 3m float rate (not defined on FX discount)
 */

class DiscountCurve(val currency:Currency, val zc : YieldParameter, val discountspread : YieldParameter, val daycount:DayCounter) {
  def this(c:Currency, v:YieldParameter) = this(c, v, null, new Thirty360)
  val valuedate = zc.valuedate
  
  def toZCImpliedYieldTermStructure = new ZCImpliedYieldTermStructure(this)
  def toZCImpliedYieldTermStructure(calendar:Calendar) = new ZCImpliedYieldTermStructure(this, calendar)
  
  def toDiscountBondEngine = new DiscountingBondEngine(this.toZCImpliedYieldTermStructure)
  def toDiscountBondEngine(calendar:Calendar) = new DiscountingBondEngine(this.toZCImpliedYieldTermStructure(calendar))
}

