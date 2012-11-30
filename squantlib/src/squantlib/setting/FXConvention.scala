package squantlib.setting

import squantlib.model.fx._
import squantlib.model.rates.DiscountCurve
import squantlib.database.schemadefinitions.RateFXParameter

/**
 * Initializes default FX volatility model
 */
object FXConvention {
  
	def getFX(underlying:String, curveDom:DiscountCurve, curveFor:DiscountCurve, params:Set[RateFXParameter]):Option[FX] = {
	  None
	}
	
	def getZeroVol(curveDom:DiscountCurve, curveFor:DiscountCurve) = FXzeroVol(curveDom, curveFor)
	
}

object USDJPYConvention {
	
}