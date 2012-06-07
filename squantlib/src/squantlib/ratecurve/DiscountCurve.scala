package squantlib.ratecurve

import squantlib.parameter.TimeVector
import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }
import org.jquantlib.time.Frequency
import org.jquantlib.daycounters.DayCounter

/**
 * Encapsulates a curve to be used for cashflow discounting, using either rates, swap points or any other means.
 */
trait DiscountableCurve {
  
  /** 
   * Builds zero coupon curve using the curve itself as discount currency.
   * @param refinance spread on float rate
   */
  def getZC(spread:TimeVector) : DiscountCurve

  /** 
   * Builds zero coupon curve using external curve as discount currency.
   * Either external curve or this curve must be basis swap pivot curve.
   * @param refinance spread on float rate
   */
  def getZC(discountrate:RateCurve, discountcurve:DiscountCurve) : DiscountCurve
}


/**
 * Encapsulates cashflow discounting curve and discount spread on 3m float rate.
 * value = discount factor, ZCspread = discount spread on 3m float rate (not defined on FX discount)
 */


class DiscountCurve(val zc : TimeVector, val discountspread : TimeVector) {
  def this(v:TimeVector) = this(v, null)
}

