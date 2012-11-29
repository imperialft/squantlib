package squantlib.model.fx

import squantlib.model.rates.DiscountCurve

/**
 * FX with time & strike dependent volatility.
 * 
 */


class FXsmiled(val curveDom:DiscountCurve, val curveFor:DiscountCurve, vol:(Double, Double) => Double) extends FX {
  
	def volatility(days:Double, strike:Double):Double = vol(days, strike)
}

object FXsmiled {
  
	def apply(curve1:DiscountCurve, curve2:DiscountCurve, vol:(Double, Double) => Double):Option[FXsmiled] = Some(new FXsmiled(curve1, curve2, vol))

}

