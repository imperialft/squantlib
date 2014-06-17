package net.squantlib.model.fx

import net.squantlib.model.rates.DiscountCurve

/**
 * FX with flat volatility for all maturities.
 * 
 */



class FXflatVol(val curveDom:DiscountCurve, val curveFor:DiscountCurve, vol:Double) extends FX {
  
	override def volatility(days:Double):Double = vol
	override def volatility(days:Double, strike:Double):Double = vol

}

object FXflatVol {
  
	def apply(curveDom:DiscountCurve, curveFor:DiscountCurve, vol:Double):Option[FXflatVol] = vol match {
	  case v if v.isNaN || v.isInfinity => None
	  case v => Some(new FXflatVol(curveDom, curveFor, v))
	}

}


