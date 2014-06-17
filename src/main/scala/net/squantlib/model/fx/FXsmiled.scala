package net.squantlib.model.fx

import net.squantlib.model.rates.DiscountCurve

/**
 * FX with time & strike dependent volatility.
 * 
 */


class FXsmiled(val curveDom:DiscountCurve, val curveFor:DiscountCurve, vol:(Double, Double) => Double) extends FX {
  
	override def volatility(days:Double):Double = vol(days, forward(days))
	override def volatility(days:Double, strike:Double):Double = vol(days, strike)
}

object FXsmiled {
  
	def apply(curve1:DiscountCurve, curve2:DiscountCurve, vol:(Double, Double) => Double):Option[FXsmiled] = Some(new FXsmiled(curve1, curve2, vol))

}

