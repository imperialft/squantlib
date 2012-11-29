package squantlib.model.fx

import squantlib.model.rates.DiscountCurve

/**
 * FX with flat volatility for all maturities.
 * 
 */



class FXconstantVol(val curveDom:DiscountCurve, val curveFor:DiscountCurve, vol:Double) extends FX {
  
	def volatility(days:Double, strike:Double):Double = vol

}

object FXconstantVol {
  
	def apply(curve1:DiscountCurve, curve2:DiscountCurve, vol:Double):Option[FXconstantVol] = Some(new FXconstantVol(curve1, curve2, vol))

}


