package squantlib.model.discountcurve

import squantlib.parameter.yieldparameter.YieldParameter
import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.America.USDCurrency
import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }


/**
 * Encapsulates a full FX curve. Should implement getZC() in superclass.
 */
trait FXCurve extends DiscountableCurve{
  val currency : Currency
  val pivotcurrency : Currency
  val fx : Double
  val swappoint : SwapPointCurve
  val valuedate : JDate
  
	    /**
	   * View
	   */
  def describecontent:Unit = {
	  val rounding = (x: Double, decimals:Int) => (x * math.pow(10, decimals)).round / math.pow(10, decimals)
	  val percent = (x:Double, decimals:Int) => (rounding(x*100, decimals)) + "%"
	  val vdescribe = (v : YieldParameter) => { "value " + v.valuedate.shortDate.toString + ":" + percent(v.value(v.valuedate), 2) + " to " + v.maxdate.shortDate.toString + ":" + percent(v.value(v.maxdays), 2) }
	  val swapptdescribe = (r : SwapPointCurve) => " quoted vs " + r.pivotcurrency.code + " multiplier:" + r.multiplier
	  
	  println("***" + currency.code + "***")
	  println("fx: " + fx)
	  println("points: "  + vdescribe(swappoint.points))  
  }
}


/**
 * Swap point curve
 * 
 * @constructor stores each information
 * @param swap point against pivot currency = USD
 * @param multiple to make up 1 unit of FX. ie. forwardfx = spotfx + swap point / multiplier
 */
class SwapPointCurve (val points:YieldParameter, val multiplier:Double, val currency:Currency, val pivotcurrency:Currency) {
  require (currency != SwapPointCurve.pivotcurrency && pivotcurrency == SwapPointCurve.pivotcurrency)
  
  val valuedate = points.valuedate

  /** 
   * Returns multiplier-adjusted swap point for the given date.
   */
  def value(d:JDate) = points.value(d) / multiplier
  def value(d:JPeriod) = points.value(d) / multiplier
  def value(d:Long) = points.value(d) / multiplier
  
  /** 
   * Returns forward fx for the given date.
   * @param spot fx
   */
  def value(d:JDate, fx:Double) = fx + points.value(d) / multiplier
  def value(d:JPeriod, fx:Double) = fx + points.value(d) / multiplier
  def value(d:Long, fx:Double) = fx + points.value(d) / multiplier
  
  def this(p:YieldParameter, m:Double, c:Currency) = this(p, m, c, SwapPointCurve.pivotcurrency)
}

object SwapPointCurve {
  val pivotcurrency = new USDCurrency
}
