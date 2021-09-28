package net.squantlib.math.volatility

import net.squantlib.model.rates.DiscountCurve
import net.squantlib.model.yieldparameter.{YieldParameter, YieldParameter3D}
import net.squantlib.util.ql.{Date => qlDate, Period => qlPeriod}
import net.squantlib.math.statistical.NormSInv
import net.squantlib.math.solver._
import org.apache.commons.math3.distribution._

case class FxBlackOption(
    spot:Double,
    atmVolatility:Double => Double,
    rateDom:Double => Double,
    rateFor:Double => Double
  ) {

  val nd = new NormalDistribution

  def d1(expiry:Double, strike:Double) = {
    val vol = atmVolatility(expiry)
    (math.log(forward(expiry) / strike) + vol * vol / 2.0 * expiry) / (vol * math.sqrt(expiry))
  }

  def d2(expiry:Double, strike:Double) = {
    val vol = atmVolatility(expiry)
    (math.log(forward(expiry) / strike) - vol * vol / 2.0 * expiry) / (vol * math.sqrt(expiry))
  }

  def forwardPrice(expiry:Double, strike:Double, isCall:Boolean = true) = {
    val cp = callPut(isCall)
    cp * (forward(expiry) * NormSInv.cumulativeNormalDist(cp * d1(expiry, strike)) - strike * NormSInv.cumulativeNormalDist(cp * d2(expiry, strike)))
  }

  def price(expiry:Double, strike:Double, isCall:Boolean = true) = forwardPrice(expiry, strike, isCall) * math.exp(-rateDom(expiry) * expiry)

  def deltaNeutralStrike(expiry:Double):Double = {
    val vol = atmVolatility(expiry)
    forward(expiry) * math.exp(0.5 * vol * vol * expiry)
  }

  def forward(expiry:Double):Double = math.exp(-(rateFor(expiry) - rateDom(expiry)) * expiry) * spot

  def forwardDelta(expiry:Double, strike:Double, premiumAdjusted:Boolean, isCall:Boolean = true):Double = {
    if (premiumAdjusted) callPut(isCall) * strike / forward(expiry) * NormSInv.cumulativeNormalDist(callPut(isCall) * d2(expiry, strike))
    else callPut(isCall) * NormSInv.cumulativeNormalDist(callPut(isCall) * d1(expiry, strike))
  }

  def spotDelta(expiry:Double, strike:Double, premiumAdjusted:Boolean, isCall:Boolean = true):Double = forwardDelta(expiry, strike, premiumAdjusted) * math.exp(-expiry * rateFor(expiry))

  private def callPut(isCall:Boolean):Double = if(isCall) 1.0 else -1.0

  def spotDeltaToStrike(expiry:Double, delta:Double, premiumAdjusted:Boolean, isCall:Boolean = true, accuracy:Double = spot * 0.0005, maxIteration:Int = 100):Option[Double] = {
    val fwd = forward(expiry)
    val cp = callPut(isCall)
    val vol = atmVolatility(expiry)
    val df = math.exp(rateFor(expiry) * expiry)
    val nonPremAdjusted = fwd * math.exp(-cp * vol * math.sqrt(expiry) * NormSInv(cp * delta * df) + 0.5 * vol * vol * expiry)
    if (premiumAdjusted) {
      val deltaFormula = (k:Double) => cp * df * k / forward(expiry) * NormSInv.cumulativeNormalDist(cp * ((math.log(fwd / k) - 0.5 * vol * vol * expiry) / (vol * math.sqrt(expiry)))) - delta

      val minRangeFormula = (k:Double) => {
        val ld2 = d2(expiry, k)
        nd.density(ld2) - vol * math.sqrt(expiry) * NormSInv.cumulativeNormalDist(ld2)
      }

      val minRange = Brent.solve(minRangeFormula, 0.0, nonPremAdjusted, spot * 0.001, maxIteration).getOrElse(0.0)

      Brent.solve(deltaFormula, minRange, nonPremAdjusted, accuracy, maxIteration)
    } else Some(nonPremAdjusted)
  }

  def forwardDeltaToStrike(expiry:Double, delta:Double, premiumAdjusted:Boolean, isCall:Boolean = true, accuracy:Double = spot * 0.0005, maxIteration:Int = 100):Option[Double] = {
    val fwd = forward(expiry)
    val cp = callPut(isCall)
    val vol = atmVolatility(expiry)
    val nonPremAdjusted = fwd * math.exp(-cp * vol * math.sqrt(expiry) * NormSInv(cp * delta) + 0.5 * vol * vol * expiry)
    if (premiumAdjusted) {
      val deltaFormula = (k: Double) => cp * k / forward(expiry) * NormSInv.cumulativeNormalDist(cp * ((math.log(fwd / k) - 0.5 * vol * vol * expiry) / (vol * math.sqrt(expiry)))) - delta

      if (isCall) {
        val minRangeFormula = (k: Double) => {
          val ld2 = d2(expiry, k)
          nd.density(ld2) - vol * math.sqrt(expiry) * NormSInv.cumulativeNormalDist(ld2)
        }

        val minRange = Brent.solve(minRangeFormula, 0.0, nonPremAdjusted, spot * 0.001, maxIteration).getOrElse(0.0)
        //      println("minRange:" + minRange + " deltaMin:"+ deltaFormula(minRange))

        Brent.solve(deltaFormula, minRange, nonPremAdjusted, accuracy, maxIteration)
      } else {
        Brent.solve(deltaFormula, 0.0, nonPremAdjusted, accuracy, maxIteration)
      }

    } else Some(nonPremAdjusted)
  }

  def solveDeltaToStrike(expiry:Double, delta:Double, premiumAdjusted:Boolean, isCall:Boolean = true, accuracy:Double = 0.005, maxIteration:Int = 100):Option[Double] = {
    NewtonRaphson.solve((k:Double) => spotDelta(expiry, k, premiumAdjusted, isCall) - delta, spot, accuracy, spot * 0.1, maxIteration)
  }

  def smiledCall(expiry:Double, delta:Double, riskReversal:Double, strangle:Double, forwardDelta:Boolean = true, premiumAdjusted:Boolean = true):Option[(Double, Double)] = {
    val volCall = atmVolatility(expiry) + 0.5 * riskReversal + strangle
    val strikeCall = if (forwardDelta) forwardDeltaToStrike(expiry, delta, premiumAdjusted, true) else spotDeltaToStrike(expiry, delta, premiumAdjusted, true)
//    println("expiry:" + expiry + " delta:" + delta + " RR:" + riskReversal + " fwdDt:" + forwardDelta + " premAdj:" + premiumAdjusted + " volC:" + volCall + " kCall:" + strikeCall)
    strikeCall match {
      case Some(k) => Some((k, volCall))
      case _ => None
    }
  }

  def smiledPut(expiry:Double, delta:Double, riskReversal:Double, strangle:Double, forwardDelta:Boolean = true, premiumAdjusted:Boolean = true):Option[(Double, Double)] = {
    val volCall = atmVolatility(expiry) - 0.5 * riskReversal + strangle
    val strikeCall = if (forwardDelta) forwardDeltaToStrike(expiry, delta, premiumAdjusted, false) else spotDeltaToStrike(expiry, delta, premiumAdjusted, false)
//    println("expiry:" + expiry + " delta:" + delta + " RR:" + riskReversal + " fwdDt:" + forwardDelta + " premAdj:" + premiumAdjusted + " volC:" + volCall + " kCall:" + strikeCall)
    strikeCall match {
      case Some(k) => Some((k, volCall))
      case _ => None
    }
  }

  def smiledParametersToVols(expiry:Double, deltaRrStrangle:Set[(Double, Double, Double)], forwardDelta:Boolean = false, premiumAdjusted:Boolean = true):Set[(Double, Double)] = {
    val atmVol:(Double, Double) = (deltaNeutralStrike(expiry), atmVolatility(expiry))

    val smiledCalls:Set[(Double, Double)] = deltaRrStrangle.map { case (delta, riskReversal, strangle) =>
      val volCall = atmVolatility(expiry) + 0.5 * riskReversal + strangle
      val strikeCall = if (forwardDelta) forwardDeltaToStrike(expiry, delta, premiumAdjusted, true) else spotDeltaToStrike(expiry, delta, premiumAdjusted, true)
      (strikeCall, volCall)
    }.collect{case (Some(k), v) => (k, v)}

    val smiledPuts:Set[(Double, Double)] = deltaRrStrangle.map { case (delta, riskReversal, strangle) =>
      val volPut = atmVolatility(expiry) - 0.5 * riskReversal + strangle
      val strikePut = if (forwardDelta) forwardDeltaToStrike(expiry, -delta, premiumAdjusted, false) else spotDeltaToStrike(expiry, -delta, premiumAdjusted, false)
      (strikePut, volPut)
    }.collect{case (Some(k), v) => (k, v)}

    (smiledCalls ++ smiledPuts) + atmVol
  }


}

