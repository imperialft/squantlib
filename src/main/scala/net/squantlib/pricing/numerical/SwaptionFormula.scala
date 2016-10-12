package net.squantlib.pricing.numerical

import org.apache.commons.math3.distribution.NormalDistribution

object SwaptionFormula {
  
  val normdist = new NormalDistribution
  def NormSDist(d:Double):Double = normdist.cumulativeProbability(d)
  
  /* Standard Black swaption calculation
   * @param expiry 	years to option exercise date
   * @param maturity duration of the swap
   * @param forward	forward swap rate
   * @param strike	strike rate
   * @param sigma 	Black volatility for swaption
   * @param discount cash-flow discount factor ZC such that PV = amount x ZC, for expiry date.
   * @returns price
   */
  def price(
      expiry:Double, 
      maturity:Double, 
      forward:Double, 
      strike:Double, 
      sigma:Double, 
      discount:Double,
      isPayer:Boolean):Double = {
    
    val dt = sigma * math.sqrt(expiry)
    val d1 = (math.log(forward / strike) + (0.5 * sigma * sigma) * expiry) / dt
    val d2 = d1 - dt
    val z = (1 - math.exp(-forward * maturity)) / forward
    
    if (isPayer) {
      val nd1 = NormSDist(d1)
      val nd2 = NormSDist(d2)
      z * discount * (forward * nd1 - strike * nd2)
    }
    else {
      val nnd1 = NormSDist(-d1)
      val nnd2 = NormSDist(-d2)
      z * discount * (strike * nnd2 - forward * nnd1)
    }
    
  }
  
}