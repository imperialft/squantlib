package squantlib.ratecurve

import squantlib.parameter.TimeVector

/**
 * Encapsulates a curve to be used for cashflow discounting, using either rates, swap points or any other means.
 */
trait DiscountableCurve {
  
  /** 
   * Builds zero coupon curve using the curve itself as discount currency.
   * @param spread on 3 month float rate
   */
  def getZC(spread:TimeVector) : DiscountCurve

  /** 
   * Builds zero coupon curve using external curve as discount currency.
   * External curve is either 
   * @param spread on 3 month float rate
   */
  def getZC(discountRate:RateCurve, discountcurve:DiscountCurve) : DiscountCurve
}


/**
 * Encapsulates cashflow discounting curve and discount spread on 3m float rate.
 * value = discount factor, ZCspread = discount spread on 3m float rate (not defined on FX discount)
 */
class DiscountCurve(val value : TimeVector, val discountspread : TimeVector) {
  def this(v:TimeVector) = this(v, null)
}