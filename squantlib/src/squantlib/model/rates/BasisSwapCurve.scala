package squantlib.model.rates

import squantlib.model.yieldparameter.YieldParameter
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.America.USDCurrency
import org.jquantlib.indexes.IborIndex
import org.jquantlib.indexes.ibor.USDLibor
import org.jquantlib.time.{TimeUnit, Date => qlDate, Period => qlPeriod, Frequency }
import squantlib.model.yieldparameter._
import squantlib.setting.RateConvention


/**
 * Basis swap rate curve. Pivot currency is assumed to be in USD.
 * 
 * @constructor stores each information. currency information is encapsulated within float index.
 * @param daycount and frequency convention (should be quarterly with standard cash daycount)
 */
case class BasisSwapCurve (rate:YieldParameter, floatindex:IborIndex) extends AbstractCurve {
  require(floatindex.tenor().length() == 3)
  
  val currency = floatindex.currency

  val pivotcurrency = BasisSwapCurve.pivotcurrency
  val pivotfloatindex = BasisSwapCurve.pivotFloatIndex
  val ispivotcurrency = currency == pivotcurrency
  
  def shifted(shift:(Double, Double) => Double):BasisSwapCurve = new BasisSwapCurve(rate.shifted(shift), floatindex)
}

object BasisSwapCurve {
  val pivotcurrency = new USDCurrency
  val pivotFloatIndex = new USDLibor(new qlPeriod(3, TimeUnit.Months))
  
  def basis_curve(valuedate:qlDate, values:Map[qlPeriod, Double]):YieldParameter 
    = (values.keySet.size) match {
     	case 1 => FlatVector(valuedate, values)
		case 2 => LinearNoExtrapolation(valuedate, values)
		case _ => SplineNoExtrapolation(valuedate, values, 2)
		} 
	
  def apply(valuedate:qlDate, currency:String, values:Map[qlPeriod, Double]):Option[BasisSwapCurve]
	= apply(basis_curve(valuedate, values), currency)

  def apply(curve:YieldParameter, currency:String):Option[BasisSwapCurve]
    = RateConvention(currency) collect {case conv => BasisSwapCurve(curve, conv.basisFloatIndex)}
  
}

