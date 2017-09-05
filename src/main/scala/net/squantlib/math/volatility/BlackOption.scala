package net.squantlib.math.volatility

import net.squantlib.model.rates.DiscountCurve
import net.squantlib.model.yieldparameter.{YieldParameter, YieldParameter3D}
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod}
import net.squantlib.math.statistical.NormSInv
import net.squantlib.math.solver._

case class BlackOption(
    spot:Double,
    atmVolatility:Double => Double,
    rate:Double => Double,
    dividend:Double => Double
  ) {

  
  def delta(expiry:Double, strike:Double):Double = {
    val vol = atmVolatility(expiry)
    val d1 = (math.log(spot / strike) + (rate(expiry) - dividend(expiry) + vol * vol / 2.0) * expiry) / (vol * expiry)
    NormSInv.cumulativeNormalDist(d1)
  }

  def deltaCall(expiry:Double, strike:Double):Double = delta(strike, expiry) * math.exp(-expiry * rate(expiry))

  def deltaPut(expiry:Double, strike:Double):Double = (delta(strike, expiry) - 1.0) * math.exp(-expiry * rate(expiry))

  def solveCallStrike(expiry:Double, delta:Double, maxIteration:Int = 40):Option[Double] = {
    NewtonRaphson.solve((k:Double) => deltaCall(expiry, k) - delta, spot, 0.005, spot * 0.1, maxIteration)
  }

  def solvePutStrike(expiry:Double, delta:Double, maxIteration:Int = 40):Option[Double] = {
    NewtonRaphson.solve((k:Double) => deltaCall(expiry, k) - 1.0 + delta, spot, 0.005, spot * 0.1, maxIteration)
  }


}

