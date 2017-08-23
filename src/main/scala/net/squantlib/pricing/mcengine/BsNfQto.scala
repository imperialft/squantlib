package net.squantlib.pricing.mcengine

import net.squantlib.math.random.{RandomGenerator, MersenneTwister, ParkMiller}
import net.squantlib.math.statistical.NormSInv
import net.squantlib.model.asset.Underlying
import net.squantlib.util.DisplayUtils._
import net.squantlib.math.statistical.{Mathematical, Cholesky}
import net.squantlib.model.fx.FX
import scala.annotation.tailrec
import net.squantlib.util.DisplayUtils
import scala.collection.mutable.ListBuffer

/* Simple Black-Scholes montecarlo pricer.
 * - Continuous dividend
 * - Volatility is constant over time without smile
 * - No rate & dividend volatility
 * @param spot     current underlying price
 * @param rate(t)  continuous compounding risk-free rate of pricing currency at time t as number of years
 * @param dividendYield(t)  continuous compounding risk-free dividend yield at time t as number of years
 * @param sigma(t)  volatility of the underlying FX
 */


object BsNfQto {

  def apply(uls:List[Underlying], fxs:List[Option[FX]]):Option[BsNfQtoAtm] = getAtm(uls, fxs)
  
  def getAtm(uls:List[Underlying], fxs:List[Option[FX]]):Option[BsNfQtoAtm] = {
    try {
      val variables: List[String] = uls.map(_.id)
      val spots: List[Double] = uls.map(_.spot)
      val rates: Double => Double = uls.head.discountRateY
      val dividendyield: List[Double => Double] = uls.map(ul => {
        val q: Double => Double = ul.assetYieldY; q
      })
      val repoyield: List[Double => Double] = uls.map(ul => {
        val q: Double => Double = ul.repoRateY; q
      })

      val dividends: List[Map[Double, Double]] = uls.map(_.dividendsY)
      val volatility: List[Double => Double] = uls.map(ul => (d: Double) => ul.volatilityY(d))
      val correl: Array[Array[Double]] = uls.map(ul => uls.map(u => u.impliedCorrelation(ul).getOrElse(Double.NaN)).toArray).toArray

      if (correl.exists(c => c.exists(d => d.isNaN))) {
        return None
      }

      val isQto = fxs.map(_.isDefined)

      val fxvol: List[Double => Double] = fxs.map {
        case Some(fx) => (d: Double) => fx.volatilityY(d)
        case None => (d: Double) => Double.NaN
      }

      val correlfx: List[Double] = (uls, fxs).zipped.map {
        case (ul, Some(fx)) => -ul.genericHistoricalCorrel(fx).getOrElse(Double.NaN)
        case (ul, None) => 0.0
      }

      if (correlfx.exists(_.isNaN)) {
        return None
      }

      Some(new BsNfQtoAtm(variables, spots, rates, dividendyield, repoyield, dividends, volatility, correl, isQto, fxvol, correlfx))
    }
    catch {
      case _: Throwable => None
    }
  }

  def getLocalVol(uls:List[Underlying], fxs:List[Option[FX]]):Option[BsNfQtoLocalVol] =
    if (uls.forall(ul => ul.isSmiledVol)) {
      try {
        val variables: List[String] = uls.map(_.id)
        val spots: List[Double] = uls.map(_.spot)
        val rates: Double => Double = uls.head.discountRateY
        val dividendyield: List[Double => Double] = uls.map(ul => {
          val q: Double => Double = ul.assetYieldY; q
        })
        val repoyield: List[Double => Double] = uls.map(ul => {
          val q: Double => Double = ul.repoRateY; q
        })

        val dividends: List[Map[Double, Double]] = uls.map(_.dividendsY)
        val localVolatility: List[(Double, Double) => Double] = uls.map(ul => (d: Double, k:Double) => ul.localVolatilityY(d, k))
        val correl: Array[Array[Double]] = uls.map(ul => uls.map(u => u.impliedCorrelation(ul).getOrElse(Double.NaN)).toArray).toArray

        if (correl.exists(c => c.exists(d => d.isNaN))) {
          return None
        }

        val isQto = fxs.map(_.isDefined)

        val fxvol: List[Double => Double] = fxs.map {
          case Some(fx) => (d: Double) => fx.volatilityY(d)
          case None => (d: Double) => Double.NaN
        }

        val correlfx: List[Double] = (uls, fxs).zipped.map {
          case (ul, Some(fx)) => -ul.genericHistoricalCorrel(fx).getOrElse(Double.NaN)
          case (ul, None) => 0.0
        }

        if (correlfx.exists(_.isNaN)) {
          return None
        }

        Some(new BsNfQtoLocalVol(variables, spots, rates, dividendyield, repoyield, dividends, localVolatility, correl, isQto, fxvol, correlfx))
      }
      catch {
        case _: Throwable => None
      }
    } else None
}

