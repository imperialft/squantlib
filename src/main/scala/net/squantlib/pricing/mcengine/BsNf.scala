package net.squantlib.pricing.mcengine

import net.squantlib.math.random.{RandomGenerator, MersenneTwister, ParkMiller}
import net.squantlib.math.statistical.NormSInv
import net.squantlib.model.asset.Underlying
import net.squantlib.util.DisplayUtils._
import net.squantlib.math.statistical.{Mathematical, Cholesky}
import scala.annotation.tailrec
import net.squantlib.util.DisplayUtils

/* Simple Black-Scholes montecarlo pricer.
 * - Continuous dividend
 * - Volatility is constant over time without smile
 * - No rate & dividend volatility
 * @param spot     current underlying price
 * @param rate(t)  continuous compounding risk-free rate of pricing currency at time t as number of years
 * @param dividendYield(t)  continuous compounding risk-free dividend yield at time t as number of years
 * @param sigma(t)  volatility of the underlying FX
 */

object BsNf {
  
  def apply(uls:List[Underlying]):Option[BsNfAtm] = getAtm(uls)
  
  def getAtm(uls:List[Underlying]):Option[BsNfAtm] = 
    try {
      val variables:List[String] = uls.map(_.id)
      val spots:List[Double] = uls.map(_.spot)
      val rates:Double => Double = uls.head.discountRateY
      val dividendyield:List[Double => Double] = uls.map(ul => {val q:Double => Double = ul.assetYieldY; q})
      val repoyield:List[Double => Double] = uls.map(ul => {val q:Double => Double = ul.repoRateY; q})
  
      val dividends:List[Map[Double, Double]] = uls.map(_.dividendsY)
      val volatility:List[Double => Double] = uls.map(ul => (d:Double) => ul.volatilityY(d))
      val correl:Array[Array[Double]] = uls.map(ul => uls.map(u => u.impliedCorrelation(ul).getOrElse(Double.NaN)).toArray).toArray
      
      if (correl.exists(c => c.exists(d => d.isNaN))) None
      else Some(new BsNfAtm(variables, spots, rates, dividendyield, repoyield, dividends, volatility, correl)) 
     } 
    catch { case _:Throwable => None}

  def getLocalVol(uls:List[Underlying]):Option[BsNfLocalVol] = 
    if (uls.forall(ul => ul.isSmiledVol)) {
      try {
        val variables:List[String] = uls.map(_.id)
        val spots:List[Double] = uls.map(_.spot)
        val rates:Double => Double = uls.head.discountRateY
        val dividendyield:List[Double => Double] = uls.map(ul => {val q:Double => Double = ul.assetYieldY; q})
        val repoyield:List[Double => Double] = uls.map(ul => {val q:Double => Double = ul.repoRateY; q})
    
        val dividends:List[Map[Double, Double]] = uls.map(_.dividendsY)
        val localVolatility:List[(Double, Double) => Double] = uls.map(ul => (d:Double, k:Double) => ul.localVolatilityY(d, k))
        val correl:Array[Array[Double]] = uls.map(ul => uls.map(u => u.impliedCorrelation(ul).getOrElse(Double.NaN)).toArray).toArray
        
        if (correl.exists(c => c.exists(d => d.isNaN))) None
        else Some(new BsNfLocalVol(variables, spots, rates, dividendyield, repoyield, dividends, localVolatility, correl)) 
       } 
      catch { case _:Throwable => None}
    } else None
    
}

