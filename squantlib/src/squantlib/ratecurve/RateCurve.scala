package squantlib.ratecurve

import squantlib.parameter.TimeVector
import org.jquantlib.daycounters._
import org.jquantlib.time.Frequency
import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.America.USDCurrency
import org.jquantlib.indexes.IborIndex
import org.jquantlib.indexes.ibor.USDLibor
import org.jquantlib.time.TimeUnit;
import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }

/**
 * Encapsulates a full rate curve. Should implement getZC() in superclass.
 */
trait RateCurve extends DiscountableCurve{
  val currency : Currency
  val cash : CashCurve
  val swap : SwapCurve
  val basis : BasisSwapCurve
  val tenorbasis : TenorBasisSwapCurve
  val valuedate : JDate
}

trait AbstractRateCurve{
  val rate : TimeVector
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
class SwapCurve (val rate:TimeVector, val floatindex:IborIndex, val fixdaycount:DayCounter, val fixperiod:Frequency) extends AbstractRateCurve{
  require (floatindex.tenor().units() == TimeUnit.Months && List(3, 6).contains(floatindex.tenor().length()))
  val currency = floatindex.currency
}


/**
 * Cash rate curve
 * 
 * @constructor stores each information
 * @param floatindex can take any maturity.
 */
class CashCurve (val rate:TimeVector, val floatindex:IborIndex) extends AbstractRateCurve{
    val currency = floatindex.currency
}


/**
 * Basis swap rate curve. Pivot currency is assumed to be in USD.
 * 
 * @constructor stores each information. currency information is encapsulated within float index.
 * @param daycount and frequency convention (should be quarterly with standard cash daycount)
 */
class BasisSwapCurve (val rate:TimeVector, val floatindex:IborIndex) extends AbstractRateCurve {
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
class TenorBasisSwapCurve (val rate:TimeVector, val shortindex:IborIndex, val longindex:IborIndex) extends AbstractRateCurve  {
  require(shortindex.tenor().length == 3 && longindex.tenor().length == 6 && shortindex.currency == longindex.currency)
  val currency = shortindex.currency
}

