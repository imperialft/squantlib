package net.squantlib.pricing.mcengine

import net.squantlib.math.random.{RandomGenerator, MersenneTwister}
import net.squantlib.math.statistical.NormSInv
import net.squantlib.model.fx.FX
import net.squantlib.util.DisplayUtils._
import scala.annotation.tailrec
import net.squantlib.model.index.Index
import scala.collection.mutable.ListBuffer

/* Simple Black-Scholes montecarlo pricer.
 * - Continuous dividend
 * - Volatility is constant over time without smile
 * - No rate & dividend volatility
 * @param spot 		current underlying price
 * @param rate(t)	continuous compounding risk-free rate of pricing currency at time t as number of years
 * @param dividendYield(t)	continuous compounding risk-free dividend yield at time t as number of years
 * @param sigma(t)	volatility of the underlying FX
 */

object Bs1fContinuous {

  def apply(fx:FX):Option[Bs1fContinuousAtm] = getAtm(fx)
  def apply(index:Index):Option[Bs1fContinuousAtm] = getAtm(index)
  
  def getAtm(fx:FX):Option[Bs1fContinuousAtm] = 
    try { Some(new Bs1fContinuousAtm(fx.spot, fx.rateDomY, fx.rateForY, fx.volatilityY)) } 
    catch { case _:Throwable => None}
	
  def getAtm(index:Index):Option[Bs1fContinuousAtm] = 
    try { Some(new Bs1fContinuousAtm(index.spot, index.discountRateY, d => index.dividendYieldY(d) + index.repoRateY(d), index.volatilityY)) } 
    catch { case _:Throwable => None}

  def getLocalVol(fx:FX):Option[Bs1fContinuousLocalVol] = 
    if (fx.isSmiledVol) {
      try { Some(new Bs1fContinuousLocalVol(fx.spot, fx.rateDomY, fx.rateForY, fx.localVolatilityY)) } 
      catch { case _:Throwable => None}
    } else None
  
  def getLocalVol(index:Index):Option[Bs1fContinuousLocalVol] = 
    if (index.isSmiledVol) {
      try { Some(new Bs1fContinuousLocalVol(index.spot, index.discountRateY, d => index.dividendYieldY(d) + index.repoRateY(d), index.localVolatilityY)) } 
      catch { case _:Throwable => None}
    } else None
    
}
