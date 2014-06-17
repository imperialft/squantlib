package net.squantlib.model.rates

import net.squantlib.model.yieldparameter.YieldParameter
import net.squantlib.util.Date

/**
 * Encapsulates a full rate curve. Should implement getZC() in superclass.
 */
trait RateCurve extends DiscountableCurve{
  val cash : CashCurve
  val swap : SwapCurve
  val basis : BasisSwapCurve
  val tenorbasis : TenorBasisSwapCurve
  val valuedate : Date
  
    /**
   * View
   */
  def convention:String = {
	  val rounding = (x: Double, decimals:Int) => (x * math.pow(10, decimals)).round / math.pow(10, decimals)
	  val cashdescribe = (r : CashCurve) => r.currency.code + " " + r.floatindex.dayCounter
	  val swapdescribe = (r : SwapCurve) => r.currency.code + " " + r.fixperiod.tenor + "m " + r.fixdaycount.name + " vs " + r.floatindex.tenor.length + "m " + r.floatindex.dayCounter.name
	  val basisdescribe = (r : BasisSwapCurve) => r.currency.code + " " + r.floatindex.tenor.length + "m " + r.floatindex.dayCounter.name + " vs " + r.pivotfloatindex.currency.code + " " + r.pivotfloatindex.tenor.length + "m " + r.pivotfloatindex.dayCounter.name
	  val basis36describe = (r : TenorBasisSwapCurve) => r.currency.code + " " + r.shortindex.tenor.length + "m " + r.shortindex.dayCounter.name + " vs " + " " + r.longindex.tenor.length + "m " + r.longindex.dayCounter.name
	  Array(cash.currency.code, "cash: " + cashdescribe(cash), "swap: " + swapdescribe(swap), "bs: " + basisdescribe(basis), "bs3m6m: " + basis36describe(tenorbasis)).mkString(sys.props("line.separator"))
  }
  
  def describe = (cash.currency.code + " : " + fx + " : " + valuedate.toString + " - " + swap.rate.maxdate.toString +  
      (if (cash != null) " cash" else "") + (if (swap != null) " swap" else "") +
      (if (basis != null) " basis" else "") + (if (tenorbasis != null) " bs3m6m" else ""))
  
}

trait AbstractCurve extends YieldParameter {
  val rate : YieldParameter
  def value(d:Double) = rate(d)
  var valuedate = rate.valuedate
  val mindays = rate.mindays
  val maxdays = rate.maxdays
}

