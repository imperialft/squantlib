package squantlib.model.discountcurve

import squantlib.parameter.yieldparameter.YieldParameter


/**
 * Encapsulates cashflow discounting curve and discount spread on 3m float rate.
 * value = discount factor, ZCspread = discount spread on 3m float rate (not defined on FX discount)
 */


class DiscountCurve(val zc : YieldParameter, val discountspread : YieldParameter) {
  def this(v:YieldParameter) = this(v, null)
  val valuedate = zc.valuedate
}

