package net.squantlib.model.rates

import net.squantlib.model.yieldparameter.YieldParameter
import org.jquantlib.currencies.Currency
import net.squantlib.util.Date

/**
 * Encapsulates a full FX curve. Should implement getZC() in superclass.
 */
trait FXCurve extends DiscountableCurve{
  val currency : Currency
  val pivotcurrency : Currency
  val fx : Double
  val swappoint : SwapPointCurve
  val valuedate : Date
  
    /**
   * View
   */
  def convention:String = {
	  val rounding = (x: Double, decimals:Int) => (x * math.pow(10, decimals)).round / math.pow(10, decimals)
	  val percent = (x:Double, decimals:Int) => (rounding(x*100, decimals)) + "%"
	  val swapptdescribe = (r : SwapPointCurve) => " quoted vs " + r.pivotcurrency.code + " multiplier:" + r.multiplier
	  Array(currency.code, "pivot: " + pivotcurrency.code, "fx: " + fx).mkString(sys.props("line.separator"))
  }
  
  def describe = (currency.code + " : " + fx + " : " + valuedate.toString + " - " + swappoint.points.maxdate.toString + (if (swappoint != null) " swappt" else ""))
}

