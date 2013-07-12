package squantlib.model.fx

import squantlib.model.rates.DiscountCurve
import squantlib.database.schemadefinitions.RateFXParameter
import org.jquantlib.time.{Period => qlPeriod}

/**
 * Encapsulates FX volatility parameterization.
 */

class FXInitializer (
    var vol:Map[qlPeriod, Double] = Map.empty, 
    var rr25:Map[qlPeriod, Double] = Map.empty,
	var rr10:Map[qlPeriod, Double] = Map.empty,
	var bf25:Map[qlPeriod, Double] = Map.empty,
	var bf10:Map[qlPeriod, Double] = Map.empty) {
	
	def addFXVol(v: Double):FXInitializer = new FXInitializer(vol.mapValues(_ + v), rr25, rr10, bf25, bf10)
	
	def getModel(curveDom:DiscountCurve, curveFor:DiscountCurve):Option[FX] = 
	  if (vol.size > 0 && rr25.size > 0 && rr10.size > 0 && bf25.size > 0 && bf10.size > 0) FXnoSmile(curveDom, curveFor, this)
	  else if (vol.size > 0) FXnoSmile(curveDom, curveFor, this)
	  else None
	  
	def getNoSmileModel(curveDom:DiscountCurve, curveFor:DiscountCurve):Option[FX] = 
	  if (vol.size > 0 && rr25.size > 0 && rr10.size > 0 && bf25.size > 0 && bf10.size > 0) FXnoSmile(curveDom, curveFor, this)
	  else if (vol.size > 0) FXnoSmile(curveDom, curveFor, this)
	  else None
	
	def isEmpty = vol.isEmpty
	
}

object FXInitializer {
  
	def apply(params:Set[RateFXParameter]) = getParams(params)
  
	def getParams(params:Set[RateFXParameter]):Map[String, FXInitializer] = 
	  params.groupBy(_.asset).mapValues(paramset => {
	    var fx = new FXInitializer
	    paramset.groupBy(_.instrument).map{
	      case ("FXVol", params) => fx.vol = createMap(params)
	      case ("FXVol10B", params) => fx.bf10 = createMap(params)
	      case ("FXVol10R", params) => fx.rr10 = createMap(params)
	      case ("FXVol25B", params) => fx.bf25 = createMap(params)
	      case ("FXVol25R", params) => fx.rr25 = createMap(params)
	      case _ => {}
	    }
	    if (fx.isEmpty) None else Some(fx)
	  }).collect{case (name, Some(param)) => (name, param)}
	
	private def createMap(params:Set[RateFXParameter]):Map[qlPeriod, Double] = 
	  params.map(p => (new qlPeriod(p.maturity), p.value)) (collection.breakOut)
}

