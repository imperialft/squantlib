package squantlib.model.rates

import squantlib.model.yieldparameter.YieldParameter
import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.America.USDCurrency
import org.jquantlib.time.{ Date => qlDate, Period => qlPeriod }

/**
 * Swap point curve
 * 
 * @constructor stores each information
 * @param swap point against pivot currency = USD
 * @param multiple to make up 1 unit of FX. ie. forwardfx = spotfx + swap point / multiplier
 */
class SwapPointCurve (val points:YieldParameter, val multiplier:Double, val currency:Currency, val pivotcurrency:Currency) extends YieldParameter {
  require (currency != SwapPointCurve.pivotcurrency && pivotcurrency == SwapPointCurve.pivotcurrency)
  
  var valuedate = points.valuedate

  /** 
   * Returns multiplier-adjusted swap point for the given date.
   */
  def value(d:Double) = points(d) / multiplier
  
  /** 
   * Returns forward fx for the given date.
   * @param spot fx
   */
  def value(d:qlDate, fx:Double) = fx + points(d) / multiplier
  def value(d:qlPeriod, fx:Double) = fx + points(d) / multiplier
  def value(d:Long, fx:Double) = fx + points(d) / multiplier
  
  val mindays = points.mindays
  val maxdays = points.maxdays
  
  def this(p:YieldParameter, m:Double, c:Currency) = this(p, m, c, SwapPointCurve.pivotcurrency)
  
  def dSPdR(d:Double, fx:Double, zcf:Double, currentRate:Double):Double = d / 365.0 * fx * zcf * math.exp{currentRate * d / 365.0}
  
  def shifted(shift:(Double, Double) => Double):SwapPointCurve = new SwapPointCurve(points.shifted(shift), multiplier, currency, pivotcurrency)
  
}

object SwapPointCurve {
  val pivotcurrency = new USDCurrency
}
