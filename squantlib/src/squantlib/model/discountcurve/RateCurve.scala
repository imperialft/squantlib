package squantlib.model.discountcurve

import squantlib.parameter.yieldparameter.YieldParameter
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.Frequency
import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.America.USDCurrency
import org.jquantlib.indexes.IborIndex
import org.jquantlib.indexes.ibor.USDLibor
import org.jquantlib.time.{TimeUnit, Date => JDate, Period => JPeriod }

/**
 * Encapsulates a full rate curve. Should implement getZC() in superclass.
 */
trait RateCurve extends DiscountableCurve{
  val cash : CashCurve
  val swap : SwapCurve
  val basis : BasisSwapCurve
  val tenorbasis : TenorBasisSwapCurve
  val valuedate : JDate
  
    /**
   * View
   */
  def showconvention:Unit = {
	  val rounding = (x: Double, decimals:Int) => (x * math.pow(10, decimals)).round / math.pow(10, decimals)
	  val percent = (x:Double, decimals:Int) => (rounding(x*100, decimals)) + "%"
	  val vdescribe = (v : YieldParameter) => { "value " + v.valuedate.shortDate.toString + ":" + percent(v.value(v.valuedate), 2) + " to " + v.maxdate.shortDate.toString + ":" + percent(v.value(v.maxdays), 2) }
	  val cashdescribe = (r : CashCurve) => r.currency.code + " " + r.floatindex.dayCounter
	  val swapdescribe = (r : SwapCurve) => r.currency.code + " " + r.fixperiod.tenor + "m " + r.fixdaycount.name + " vs " + r.floatindex.tenor.length + "m " + r.floatindex.dayCounter.name
	  val basisdescribe = (r : BasisSwapCurve) => r.currency.code + " " + r.floatindex.tenor.length + "m " + r.floatindex.dayCounter.name + " vs " + r.pivotfloatindex.currency.code + " " + r.pivotfloatindex.tenor.length + "m " + r.pivotfloatindex.dayCounter.name
	  val basis36describe = (r : TenorBasisSwapCurve) => r.currency.code + " " + r.shortindex.tenor.length + "m " + r.shortindex.dayCounter.name + " vs " + " " + r.longindex.tenor.length + "m " + r.longindex.dayCounter.name
	  println(cash.currency.code)
	  println("cash: " + cashdescribe(cash))
	  println("swap: " + swapdescribe(swap))
	  println("bs: " + basisdescribe(basis))
	  println("bs3m6m: " + basis36describe(tenorbasis))
  }
  
  def describe = println(cash.currency.code + " : " + valuedate.shortDate + " - " + swap.rate.maxdate.shortDate +  
      (if (cash != null) " cash" else "") + (if (swap != null) " swap" else "") +
      (if (basis != null) " basis" else "") + (if (tenorbasis != null) " bs3m6m" else ""))
  
  override def toString():String = cash.currency.code + ":ratecurve"
}

trait AbstractRateCurve{
  val rate : YieldParameter
  def value(d:JDate) = rate.value(d)
  def value(d:JPeriod) = rate.value(d)
  def value(d:Long) = rate.value(d)
  def valuedate = rate.valuedate
}

/**
 * Swap rate curve
 * 
 * @constructor stores each information
 * @param float index, daycount & payment frequency for fixed leg
 */
class SwapCurve (val rate:YieldParameter, val floatindex:IborIndex, val fixdaycount:DayCounter, val fixperiod:Frequency) extends AbstractRateCurve{
  require (floatindex.tenor().units() == TimeUnit.Months && List(3, 6).contains(floatindex.tenor().length()))
  val currency = floatindex.currency
}


/**
 * Cash rate curve
 * 
 * @constructor stores each information
 * @param floatindex can take any maturity.
 */
class CashCurve (val rate:YieldParameter, val floatindex:IborIndex) extends AbstractRateCurve{
    val currency = floatindex.currency
}


/**
 * Basis swap rate curve. Pivot currency is assumed to be in USD.
 * 
 * @constructor stores each information. currency information is encapsulated within float index.
 * @param daycount and frequency convention (should be quarterly with standard cash daycount)
 */
class BasisSwapCurve (val rate:YieldParameter, val floatindex:IborIndex) extends AbstractRateCurve {
  require(floatindex.tenor().length() == 3)
  
  val currency = floatindex.currency

  val pivotcurrency = BasisSwapCurve.pivotcurrency
  val pivotfloatindex = BasisSwapCurve.pivotFloatIndex
  val ispivotcurrency = currency == pivotcurrency
}

object BasisSwapCurve {
  val pivotcurrency = new USDCurrency
  val pivotFloatIndex = new USDLibor(new JPeriod(3, TimeUnit.Months))
}


/**
 * Tenor basis swap rate curve.
 * Currently restricted to basis 3 months vs 6 months, quoted as 3 months spread against 6 months in same currency.
 * 
 * @constructor stores each information
 * @param daycount and frequency convention (should be quarterly with standard cash daycount)
 */
class TenorBasisSwapCurve (val rate:YieldParameter, val shortindex:IborIndex, val longindex:IborIndex) extends AbstractRateCurve  {
  require(shortindex.tenor().length == 3 && longindex.tenor().length == 6 && shortindex.currency == longindex.currency)
  val currency = shortindex.currency
}

