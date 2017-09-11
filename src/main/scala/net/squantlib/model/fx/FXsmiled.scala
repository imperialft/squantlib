package net.squantlib.model.fx

import net.squantlib.model.rates.DiscountCurve
import net.squantlib.model.yieldparameter._
import net.squantlib.math.volatility.FxBlackOption
import org.jquantlib.time.{Period => qlPeriod}

/**
 * FX with time & strike dependent volatility.
 * 
 */


class FXsmiled(val curveDom:DiscountCurve, val curveFor:DiscountCurve, vol:(Double, Double) => Double) extends FX {
  
	override def volatility(days:Double):Double = vol(days, forward(days))
//	override def volatility(days:Double, strike:Double):Double = vol(days, strike)
}

object FXsmiled {

	def apply(curveDom:DiscountCurve, curveFor:DiscountCurve, params:FXInitializer):Option[FXnoSmile] = {
		assert(curveDom.valuedate == curveFor.valuedate)

		val volYield = (params.vol match {
			case v if v.isEmpty => None
			case v if v.size == 1 => Some(FlatVector(curveDom.valuedate, v))
			case v if v.size == 2 => Some(LinearNoExtrapolation(curveDom.valuedate, v))
			case v => Some(SplineNoExtrapolation(curveDom.valuedate, v, 1))
		}).orNull

		if (volYield == null) {return None} //else Some(new FXnoSmile(curveDom, curveFor, (f:Double) => volYield(f)))

    val rr25:Map[qlPeriod, Double] = params.rr25
    val rr10:Map[qlPeriod, Double] = params.rr10
    val bf25:Map[qlPeriod, Double] = params.bf25
    val bf10:Map[qlPeriod, Double] = params.bf10
    val spot = curveDom.fx / curveFor.fx

    val bs = FxBlackOption(spot, (d:Double) => volYield(d * 365.25), (d:Double) => curveDom.impliedRate(d * 365.25), (d:Double) => curveFor.impliedRate(d * 365.25))

    val exp25Smiled = rr25.keySet & bf25.keySet

    def qlPeriodToYears(p:qlPeriod):Double = (Date.days(p).toDouble / 365.0)

    val atmVols:Set[(qlPeriod, Double, Double)] = params.vol.map{case (p, v) => (p, bs.deltaNeutralStrike(qlPeriodToYears(p)), volYield(p))}.toSet

    val exp10Smiled = rr10.keySet & bf10.keySet

//    val params = indexparams.groupBy(_.instrument)
//    if (!params.contains(yieldid) || !params.contains(spotid)) {return new EmptyInitializer}
//
//    val yldparam:Map[qlPeriod, Double] = params(yieldid).map(p => {(new qlPeriod(p.maturity), p.value)}) (collection.breakOut)
//
//    val spot:Double = params(spotid).head.value
//
//    val repo:Map[qlPeriod, Double] = params.get(repoid).collect{case rs => rs.map(p => (new qlPeriod(p.maturity), p.value)).toMap}.getOrElse(Map.empty)
//
//    val atmVol:Map[qlPeriod, Double] = params.get(atmVolId).collect{case vs => vs.map(p => (new qlPeriod(p.maturity), p.value)).toMap}.getOrElse(Map.empty)
//
//    val smiledVol:Map[(qlPeriod, Double), Double] = params.get(smiledVolId).collect{case vs =>
//      vs.map(p => ((new qlPeriod(p.maturity), p.optionMap.getOrElse("k", "0.0").parseDouble.getOrElse(0.0)), p.value)).filter{case ((per, v), pv) => v > 0.00001}.toMap
//    }.getOrElse(Map.empty)
//
//    if (smiledVol.isEmpty) new IndexATMContinuous(name, ccy, spot, yldparam, repo, atmVol, discountCcy, discountSpread)
//    else new IndexSmiledContinuous(name, ccy, spot, yldparam, repo, atmVol, smiledVol, discountCcy, discountSpread)
    return null

  }


//	def apply(curve1:DiscountCurve, curve2:DiscountCurve, vol:(Double, Double) => Double):Option[FXsmiled] = Some(new FXsmiled(curve1, curve2, vol))

}

