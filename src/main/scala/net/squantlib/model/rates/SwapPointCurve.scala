package net.squantlib.model.rates

import net.squantlib.model.yieldparameter._
import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.America.USDCurrency
import org.jquantlib.time.{Period => qlPeriod }
import net.squantlib.model.rates.convention.RateConvention
import net.squantlib.util.Date

/**
 * Swap point curve
 * 
 * @constructor stores each information
 * @param swap point against pivot currency = USD
 * @param multiple to make up 1 unit of FX. ie. forwardfx = spotfx + swap point / multiplier
 */
case class SwapPointCurve (points:YieldParameter, multiplier:Double, currency:Currency, pivotcurrency:Currency) extends YieldParameter {
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
  def value(d:Date, fx:Double) = fx + points(d) / multiplier
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
  
	def buildCurve(valuedate:Date, values:Map[qlPeriod, Double]):YieldParameter	
		= (values.keySet.size) match {
			case 1 => FlatVector(valuedate, values)
			case 2 => LinearNoExtrapolation(valuedate, values)
			case _ => SplineNoExtrapolation(valuedate, values, 2)
  		} 
  
  	/**
	 * Returns tenor basis swap curve using specified conventions and curve construction method.
	 */
	def apply(valuedate:Date, currency:String, values:Map[qlPeriod, Double]):Option[SwapPointCurve]
		= apply(buildCurve(valuedate, values), currency)
  
	def apply(curve:YieldParameter, currency:String):Option[SwapPointCurve]
		= RateConvention(currency) collect { case conv => SwapPointCurve(curve, conv.swapPointMultiplier, conv.currency, conv.swapPointPivotCcy)}
  
}
