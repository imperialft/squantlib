package squantlib.model.fx

import squantlib.model.rates.DiscountCurve
import squantlib.model.yieldparameter._

/**
 * FX with time-dependent volatility. No Smile.
 */

class FXnoSmile(val curveDom:DiscountCurve, val curveFor:DiscountCurve, vol:Double => Double) extends FX {
  
	def volatility(days:Double, strike:Double):Double = vol(days)

}

object FXnoSmile {
  
	def apply(curveDom:DiscountCurve, curveFor:DiscountCurve, vol:Double => Double):Option[FXnoSmile] = Some(new FXnoSmile(curveDom, curveFor, vol))
	
	def apply(curveDom:DiscountCurve, curveFor:DiscountCurve, params:FXparameter):Option[FXnoSmile] = {
	  assert(curveDom.valuedate == curveFor.valuedate)
	  
	  val volYield = params.vol match {
	    case v if v.isEmpty => null
	    case v if v.size == 1 => new FlatVector(curveDom.valuedate, v)
	    case v if v.size == 2 => new LinearNoExtrapolation(curveDom.valuedate, v)
	    case v => new SplineNoExtrapolation(curveDom.valuedate, v, 1)
	  }
	  
	  if (volYield == null) None else Some(new FXnoSmile(curveDom, curveFor, (f:Double) => volYield(f)))
	}
}

