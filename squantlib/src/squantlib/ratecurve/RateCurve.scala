package squantlib.ratecurve

import squantlib.parameter.TimeVector
import org.jquantlib.daycounters._
import org.jquantlib.time.Frequency
import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.America.USDCurrency
import org.jquantlib.indexes.IborIndex
import org.jquantlib.time.TimeUnit;
import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.time.{ Period => JPeriod }

/**
 * Encapsulates a full rate curve. Should implement getZC() in superclass.
 */
trait RateCurve extends DiscountableCurve{
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
 * Swap rate curve with quote convention
 * 
 * @constructor stores each information
 * @param float index, daycount & payment frequency for fixed leg
 */
class SwapCurve (val rate:TimeVector, val floatIndex:IborIndex, val fixDaycount:DayCounter, val fixPeriod:Frequency) extends AbstractRateCurve{
  require (floatIndex.tenor().units() == TimeUnit.Months && List(3, 6).contains(floatIndex.tenor().length()))
}


/**
 * Cash rate curve with quote convention
 * 
 * @constructor stores each information
 * @param Daycount fraction for the lending rate
 */
class CashCurve (val rate:TimeVector, val dayCount:DayCounter) extends AbstractRateCurve


/**
 * Basis swap rate curve with quote convention. Pivot currency is assumed to be in USD.
 * 
 * @constructor stores each information. currency information is encapsulated within float index.
 * @param daycount and frequency convention (should be quarterly with standard cash daycount)
 */
class BasisSwapCurve (val rate:TimeVector, val floatIndex:IborIndex) extends AbstractRateCurve {
  val currency = floatIndex.currency

  val pivotcurrency = BasisSwapCurve.pivotcurrency
  val pivotDaycount = BasisSwapCurve.pivotDaycount
  val pivotPeriod = BasisSwapCurve.pivotPeriod

  val ispivotcurrency = currency == pivotcurrency
}

object BasisSwapCurve {
  val pivotcurrency = new USDCurrency
  val pivotDaycount = new Actual360
  val pivotPeriod = Frequency.Quarterly
}


/**
 * Tenor basis swap rate curve with quote convention, quoted as 3 months spread against 6 months in same currency.
 * 
 * @constructor stores each information
 * @param daycount and frequency convention (should be quarterly with standard cash daycount)
 */
class TenorBasisSwapCurve (val rate:TimeVector, sIndex:IborIndex, lIndex:IborIndex) extends AbstractRateCurve  {
  val shortIndex = if (sIndex.tenor().length() == 3 && sIndex.tenor().units() == TimeUnit.Months) sIndex else null
  val longIndex = if (lIndex.tenor().length() == 6 && sIndex.tenor().units() == TimeUnit.Months) sIndex else null
}

