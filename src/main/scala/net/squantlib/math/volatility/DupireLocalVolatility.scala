package net.squantlib.math.volatility

import net.squantlib.model.yieldparameter.{YieldParameter, YieldParameter3D}
import net.squantlib.model.index._
import net.squantlib.model.rates.DiscountCurve

case class DupireLocalVolatility(
   impliedVolatility: YieldParameter3D,
   rateDom: DiscountCurve,
   rateFor: Double => Double,
   spotPrice: Double
) {

  
  def localVariance(expiry:Double, strike:Double):Double = {

    val t = expiry / 365.0

    
    val sigma = impliedVolatility(expiry, strike)

    val dt = 0.1
    
    val sigDtUp = impliedVolatility(expiry + dt * 365.0, strike)

    val dSigDt = (sigDtUp - sigma) / dt


    val r = rateDom.impliedRate(expiry)


    val div = rateFor(expiry)

    
    val dk = strike * 0.01


    val sigDkUp = impliedVolatility(expiry, strike + dk)


    val dSigDk = (sigDkUp - sigma) / dk
    
    val d = (Math.log(spotPrice / strike) + (r + Math.pow(sigma, 2.0) / 2.0) * t) / (sigma * Math.sqrt(t))

    
    val sigDkNeg = impliedVolatility(expiry, strike - dk)


    val dSigDkNeg = (sigma - sigDkNeg) / dk

    val dSigDkSq = (sigDkUp + sigDkNeg - 2.0 * sigma) / Math.pow(dk * 2.0, 2.0)

    
    val denom = (Math.pow(sigma, 2.0) + 2.0 * sigma * t * (dSigDt + (r - div) * strike * dSigDk))


    val dvd = Math.pow(1.0 + strike * d * dSigDk * Math.sqrt(t), 2.0) + sigma * Math.pow(strike, 2.0) * t * (dSigDkSq - d * Math.pow(dSigDk, 2.0) * Math.sqrt(t))
    
//    if (denom / dvd < 0) {
//
//      println("----------------")
//      println("expiry \t" + expiry.toString)
//      println("strike \t" + strike.toString)
//      println("t \t" + t.toString)
//
//      println("sigma \t" + sigma.toString)
//      println("sigDtUp \t" + sigDtUp.toString)
//      println("dsigdt \t" + dSigDt.toString)
//      println("r \t" + r.toString)
//      println("div \t" + div.toString)
//      println("dk \t" + dk.toString)
//      println("sigDkUp \t" + sigDkUp.toString)
//      println("dSigDk \t" + dSigDk.toString)
//      println("d \t" + d.toString)
//      println("sigDkNeg \t" + sigDkNeg.toString)
//      println("dSigDkNeg \t" + dSigDkNeg.toString)
//      println("dSigDkSq \t" + dSigDkSq.toString)
//      println("denom \t" + denom.toString)
//      println("dvd \t" + dvd.toString)
//    }

    denom / dvd
  }
  
  def localVolatility(expiry:Double, strike:Double):Double = Math.sqrt(localVariance(expiry, strike))

  
}

object DupireLocalVolatility{

  def apply(impliedVolatility: YieldParameter3D, rateDom: DiscountCurve, dividendCurve: DividendCurve, spotPrice: Double):DupireLocalVolatility =
    DupireLocalVolatility(impliedVolatility, rateDom, (d:Double) => dividendCurve(d), spotPrice)

  def apply(impliedVolatility: YieldParameter3D, rateDom: DiscountCurve, rateFor: DiscountCurve, spotPrice: Double):DupireLocalVolatility =
    DupireLocalVolatility(impliedVolatility, rateDom, (d:Double) => rateFor.impliedRate(d), spotPrice)

}

