package net.squantlib.pricing.mcengine

import net.squantlib.math.random.{RandomGenerator, MersenneTwister}
import net.squantlib.math.statistical.NormSInv
import net.squantlib.model.fx.FX
import net.squantlib.util.DisplayUtils._
import scala.annotation.tailrec
import net.squantlib.model.index.Index

/* Simple Black-Scholes montecarlo pricer.
 * - Continuous dividend
 * - Volatility is constant over time without smile
 * - No rate & dividend volatility
 * @param spot     current underlying price
 * @param rate(t)  continuous compounding risk-free rate of pricing currency at time t as number of years
 * @param dividendYield(t)  continuous compounding risk-free dividend yield at time t as number of years
 * @param sigma(t)  volatility of the underlying FX
 */

object Bs1fQtoContinuous {

  def apply(fx:FX, qtofx:FX):Option[Bs1fQtoContinuousAtm] = getAtm(fx, qtofx)
  
  def apply(index:Index, fx:FX):Option[Bs1fQtoContinuousAtm] = getAtm(index, fx)
  
  def getAtm(fx:FX, qtofx:FX):Option[Bs1fQtoContinuousAtm] = 
  try { 
    fx.genericHistoricalCorrel(qtofx).collect{case c => 
      new Bs1fQtoContinuousAtm(fx.spot, fx.rateDomY, fx.rateForY, fx.volatilityY, qtofx.volatilityY, -c)
    }
  }
  catch { case _:Throwable => None}
  
  def getAtm(index:Index, fx:FX):Option[Bs1fQtoContinuousAtm] = 
  try { 
    index.genericHistoricalCorrel(fx).collect{case c => 
      new Bs1fQtoContinuousAtm(index.spot, index.discountRateY, d => index.dividendYieldY(d) + index.repoRateY(d), index.volatilityY, fx.volatilityY, -c)
    }
  }
  catch { case _:Throwable => None}

  def getLocalVol(fx:FX, qtofx:FX):Option[Bs1fQtoContinuousLocalVol] = 
    if (fx.isSmiledVol) {
      try { 
        fx.genericHistoricalCorrel(qtofx).collect{case c => 
          new Bs1fQtoContinuousLocalVol(fx.spot, fx.rateDomY, fx.rateForY, fx.localVolatilityY, qtofx.volatilityY, -c)
        }
      }
      catch { case _:Throwable => None}
    } else None
  
  def getLocalVol(index:Index, fx:FX):Option[Bs1fQtoContinuousLocalVol] = 
    if (index.isSmiledVol) {
      try { 
        index.genericHistoricalCorrel(fx).collect{case c => 
          new Bs1fQtoContinuousLocalVol(index.spot, index.discountRateY, d => index.dividendYieldY(d) + index.repoRateY(d), index.localVolatilityY, fx.volatilityY, -c)
        }
      }
      catch { case _:Throwable => None}
    } else None
  
  
}



