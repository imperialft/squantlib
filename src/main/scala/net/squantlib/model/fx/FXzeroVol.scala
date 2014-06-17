package net.squantlib.model.fx

import net.squantlib.model.rates.DiscountCurve

/**
 * FX with no volatility definition (always NaN).
 * 
 */

class FXzeroVol(val curveDom:DiscountCurve, val curveFor:DiscountCurve) extends FX {
  
	override def volatility(days:Double):Double = Double.NaN
	override def volatility(days:Double, strike:Double):Double = Double.NaN
	
}

object FXzeroVol {
  
	def apply(curve1:DiscountCurve, curve2:DiscountCurve):FXzeroVol = new FXzeroVol(curve1, curve2)
	
}

