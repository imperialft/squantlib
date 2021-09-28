package net.squantlib.model.fx

import net.squantlib.model.rates.DiscountCurve
import net.squantlib.model.yieldparameter._
import net.squantlib.math.volatility.FxBlackOption
import net.squantlib.util.Date
import net.squantlib.model.yieldparameter.{YieldParameter, YieldParameter3D}
import net.squantlib.util.ql.{Period => qlPeriod, Date => qlDate}
import net.squantlib.math.volatility.DupireLocalVolatility

/**
 * FX with time & strike dependent volatility.
 * 
 */


case class FXsmiled(
  val curveDom:DiscountCurve,
  val curveFor:DiscountCurve,
  atmVol: Double => Double,
  override val isSmiledVol:Boolean,
  smiledVolSurface: (Double, Double) => Double,
  localVolSurface: (Double, Double) => Double,
  val bs:FxBlackOption,
  val localVol:DupireLocalVolatility
) extends FX  {

  /**
    * Returns the volatility corresponding to the given date & strike.
    * @param days observation date as the number of calendar days after value date.
    * @param strike fx strike
    */
  override def volatility(days:Double):Double = atmVol(days)

  override def smiledVolatility(days:Double, strike:Double):Double = smiledVolSurface(days, strike)

  override def localVolatility(days:Double, strike:Double):Double = if (localVolSurface == null) Double.NaN else localVolSurface(days, strike)

  /**
    * Returns the value corresponding to the given date.
    * @param observation date as the number of calendar days after value date.
    */
}

object FXsmiled {

  def qlPeriodToDays(p:qlPeriod):Double = Date.currentDate.days(p).toDouble
  def qlPeriodToYears(p:qlPeriod):Double = (Date.currentDate.days(p).toDouble / 365.0)

	def apply(curveDom:DiscountCurve, curveFor:DiscountCurve, params:FXInitializer):Option[FX] = {
		assert(curveDom.valuedate == curveFor.valuedate)

//    println("calibrate fx smile")

    val valuedate = curveDom.valuedate

		val volYield = (params.vol match {
			case v if v.isEmpty => {
//        println("vol is empty")
        None
      }
			case v if v.size == 1 => Some(FlatVector(curveDom.valuedate, v))
			case v if v.size == 2 => Some(LinearNoExtrapolation(curveDom.valuedate, v))
			case v => Some(SplineNoExtrapolation(curveDom.valuedate, v, 1))
		}).orNull

		if (volYield == null) {
//      println("unable to calibrate atm vol")
      return None
    } //else Some(new FXnoSmile(curveDom, curveFor, (f:Double) => volYield(f)))

    val rr25:Map[qlPeriod, Double] = params.rr25
    val rr10:Map[qlPeriod, Double] = params.rr10
    val bf25:Map[qlPeriod, Double] = params.bf25
    val bf10:Map[qlPeriod, Double] = params.bf10
    val spot = curveDom.fx / curveFor.fx

    val bs = FxBlackOption(spot, (d:Double) => volYield(d * 365.25), (d:Double) => curveDom.impliedRate(d * 365.25), (d:Double) => curveFor.impliedRate(d * 365.25))

    val atmVols:Map[(Double, Double), Double] = params.vol.map{case (p, v) =>
      ((qlPeriodToDays(p), bs.deltaNeutralStrike(qlPeriodToYears(p))), volYield(p))
    }.toMap

    val exp25Smiled = rr25.keySet & bf25.keySet

    val vol25SmiledCall:Map[(Double, Double), Double] = exp25Smiled.map(p => {
      bs.smiledCall(
        expiry = qlPeriodToYears(p),
        delta = 0.25,
        riskReversal = rr25(p),
        strangle = bf25(p),
        forwardDelta = qlPeriodToYears(p) >= 0.98,
        premiumAdjusted = true
      ) match {
        case Some((k, v)) => Some(((qlPeriodToDays(p), k), v))
        case _ => None
      }
    }).flatMap(s => s).toMap

    val vol25SmiledPut:Map[(Double, Double), Double] = exp25Smiled.map(p => {
      bs.smiledPut(
        expiry = qlPeriodToYears(p),
        delta = -0.25,
        riskReversal = rr25(p),
        strangle = bf25(p),
        forwardDelta = qlPeriodToYears(p) >= 0.98,
        premiumAdjusted = true
      ) match {
        case Some((k, v)) => Some(((qlPeriodToDays(p), k), v))
        case _ => None
      }
    }).flatMap(s => s).toMap

    val exp10Smiled = rr10.keySet & bf10.keySet

    val vol10SmiledCall:Map[(Double, Double), Double] = exp10Smiled.map(p => {
      bs.smiledCall(
        expiry = qlPeriodToYears(p),
        delta = 0.10,
        riskReversal = rr10(p),
        strangle = bf10(p),
        forwardDelta = qlPeriodToYears(p) >= 0.98,
        premiumAdjusted = true
      ) match {
        case Some((k, v)) => Some(((qlPeriodToDays(p), k), v))
        case _ => None
      }
    }).flatMap(s => s).toMap

    val vol10SmiledPut:Map[(Double, Double), Double] = exp10Smiled.map(p => {
      bs.smiledPut(
        expiry = qlPeriodToYears(p),
        delta = -0.10,
        riskReversal = rr10(p),
        strangle = bf10(p),
        forwardDelta = qlPeriodToYears(p) >= 0.98,
        premiumAdjusted = true
      ) match {
        case Some((k, v)) => Some(((qlPeriodToDays(p), k), v))
        case _ => None
      }
    }).flatMap(s => s).toMap

    val inputVols = atmVols ++ vol25SmiledCall ++ vol25SmiledPut ++ vol10SmiledCall ++ vol10SmiledPut

//    println("INPUTVOL:")
//    inputVols.toList.sortBy{case ((d, k), v) => (d, k)}.foreach(p => println(p.toString))
//    println("SPOT: " + spot)


    val smiledVolCurve:YieldParameter3D = YieldParameter3D.construct(valuedate, inputVols, true).orNull
    if (smiledVolCurve == null) {
//      println("cannot calibrate smiled vol curve")
      return FXnoSmile(curveDom, curveFor, (d:Double) => volYield(d))
    }

    val localVol:DupireLocalVolatility = DupireLocalVolatility(smiledVolCurve, curveDom, curveFor, spot)
    val allStrikes:Set[Double] = (3 to 10).map(i => i.toDouble * 0.05).toSet ++ (3 to 9).map(i => -i.toDouble * 0.05).toSet
    val maturities:Set[Double] = inputVols.keys.map{case (d, v) => d}.toSet

//    println("STRIKES " + allStrikes.toString)
//    println("MATURITIES: " + maturities.toString)

    val samplePoints:Map[(Double, Double), Double] = maturities.map{case d =>
      allStrikes.map{case mness =>
        val stk = if (math.abs(mness - 0.5) < 0.0001) Some(bs.deltaNeutralStrike(d / 365.25)) else bs.forwardDeltaToStrike(d / 365.25, mness, true, mness > 0.0)
        stk match {
          case Some(k) =>
//            println(((d, k), localVol.localVolatility(d, k)).toString )
            ((d, k), localVol.localVolatility(d, k))
          case _ =>
//            println("cannot solve for d:" + d + " mness:" + mness)
            ((d, Double.NaN), Double.NaN)
        }
      }
    }.flatten.filter{case ((d, k), v) => !v.isNaN}.toMap

//    val samplePoints:Map[(Double, Double), Double] = inputVols.map{case ((d, k), v) =>
//      ((d, k), localVol.localVolatility(d, k))
//    }.filter{case ((d, k), v) => !v.isNaN}


    val filteredSample = if (samplePoints.size > 0) {
      val sampleAv = samplePoints.values.sum / samplePoints.size.toDouble
      samplePoints.filter{case ((d, k), v) => v >= sampleAv / 3.0 && v <= sampleAv * 3.0}
    } else samplePoints

    val localVolSurface = YieldParameter3D.construct(valuedate, filteredSample).orNull
    if (localVolSurface == null) {
//      println("cannot calibrate local vol curve")
      return FXnoSmile(curveDom, curveFor, (d:Double) => volYield(d))
    }

    if (filteredSample.size > 5) {
      FXsmiled(
        curveDom = curveDom,
        curveFor = curveFor,
        atmVol = volYield,
        isSmiledVol = true,
        smiledVolSurface = smiledVolCurve,
        localVolSurface = localVolSurface,
        bs = bs,
        localVol = localVol
      )
    } else {
      FXnoSmile(curveDom, curveFor, (d:Double) => volYield(d))
    }

  }

  def apply(curveDom:DiscountCurve, curveFor:DiscountCurve, atmVol:YieldParameter, isSmiledVol:Boolean, smiledVolSurface:YieldParameter3D, localVolSurface:YieldParameter3D, bs:FxBlackOption, localVol:DupireLocalVolatility):Option[FX] = {
    if (isSmiledVol) Some(FXsmiled(curveDom, curveFor, (y:Double) => atmVol(y), true, (d:Double, k:Double) => smiledVolSurface(d, k), (d:Double, k:Double) => localVolSurface(d, k), bs, localVol))
    else FXnoSmile(curveDom, curveFor, (y:Double) => atmVol(y))
  }

    //  def apply(name:String, spot:Double, rateCurve:DiscountCurve, dividend:DividendCurve, repo:RepoCurve, vol:YieldParameter3D, isSmiledVol:Boolean):SmoothIndex =
    //    SmoothIndex(name, spot, rateCurve, dividend, repo, (y:Double) => vol(y, spot), true, )



  //	def apply(curve1:DiscountCurve, curve2:DiscountCurve, vol:(Double, Double) => Double):Option[FXsmiled] = Some(new FXsmiled(curve1, curve2, vol))

}

