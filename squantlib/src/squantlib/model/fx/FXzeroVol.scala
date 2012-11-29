package squantlib.model.fx

import squantlib.model.rates.DiscountCurve

/**
 * FX with no volatility definition (always NaN).
 * 
 */

class FXzeroVol(val curveDom:DiscountCurve, val curveFor:DiscountCurve) extends FX {
  
	def volatility(days:Double, strike:Double):Double = Double.NaN
	
}

object FXzeroVol {
  
	def apply(curve1:DiscountCurve, curve2:DiscountCurve):Option[FXzeroVol] = Some(new FXzeroVol(curve1, curve2))
	
}

