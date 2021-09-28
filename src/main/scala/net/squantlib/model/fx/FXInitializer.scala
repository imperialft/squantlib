package net.squantlib.model.fx

import net.squantlib.model.rates.DiscountCurve
import net.squantlib.util.ql.{Period => qlPeriod}

/**
 * Encapsulates FX volatility parameterization.
 */

class FXInitializer (
    var vol:Map[qlPeriod, Double] = Map.empty, 
    var rr25:Map[qlPeriod, Double] = Map.empty,
		var rr10:Map[qlPeriod, Double] = Map.empty,
		var bf25:Map[qlPeriod, Double] = Map.empty,
		var bf10:Map[qlPeriod, Double] = Map.empty) {
	
	def addFXVol(v: Double):FXInitializer = {
//    println("current: " + vol.toString)
    //    println("current: " + rr10.toString)
    //    println("new: " + vol.mapValues(_ + v).toString)
		new FXInitializer(vol.mapValues(_ + v), rr25, rr10, bf25, bf10)
	}
	
	def getModel(curveDom:DiscountCurve, curveFor:DiscountCurve):Option[FX] = 
	  if (vol.size > 0 && rr25.size > 0 && rr10.size > 0 && bf25.size > 0 && bf10.size > 0) FXsmiled(curveDom, curveFor, this)
	  else if (vol.size > 0) FXnoSmile(curveDom, curveFor, this)
	  else None
	  
	def getNoSmileModel(curveDom:DiscountCurve, curveFor:DiscountCurve):Option[FX] = 
	  if (vol.size > 0 && rr25.size > 0 && rr10.size > 0 && bf25.size > 0 && bf10.size > 0) FXnoSmile(curveDom, curveFor, this)
	  else if (vol.size > 0) FXnoSmile(curveDom, curveFor, this)
	  else None
	
	def isEmpty = vol.isEmpty
	
}

