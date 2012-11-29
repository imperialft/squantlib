package squantlib.model.fx

import squantlib.model.rates.DiscountCurve

/**
 * FX with time-dependent volatility. No Smile.
 */

class FXnoSmile(val curveDom:DiscountCurve, val curveFor:DiscountCurve, vol:Double => Double) extends FX {
  
	def volatility(days:Double, strike:Double):Double = vol(days)

}

object FXnoSmile {
	def apply(curve1:DiscountCurve, curve2:DiscountCurve, vol:Double => Double):Option[FXnoSmile] = Some(new FXnoSmile(curve1, curve2, vol))
}

