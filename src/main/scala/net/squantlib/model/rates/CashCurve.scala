package net.squantlib.model.rates

import net.squantlib.model.yieldparameter.YieldParameter
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.currencies.Currency
import org.jquantlib.indexes.IborIndex
import org.jquantlib.time.{TimeUnit, Period => qlPeriod, Frequency}
import net.squantlib.util.Date
import net.squantlib.model.yieldparameter._
import net.squantlib.model.rates.convention.RateConvention

/**
 * Cash rate curve
 * 
 * @constructor stores each information
 * @param floatindex => maturity value is ignored.
 */
case class CashCurve (rate:YieldParameter, floatindex:IborIndex) extends AbstractCurve{
  val currency = floatindex.currency
  def shifted(shift:(Double, Double) => Double):CashCurve = new CashCurve(rate.shifted(shift), floatindex)
}


object CashCurve {
  
  def buildCurve(valuedate:Date, values:Map[qlPeriod, Double]):YieldParameter = values.keySet.size match {
    case 1 => FlatVector(valuedate, values)
	case 2 => LinearNoExtrapolation(valuedate, values)
	case _ => SplineNoExtrapolation(valuedate, values, 2) } 
	
  def apply(valuedate:Date, currency:String, value:Double):Option[CashCurve] = apply(valuedate, currency, Map(new qlPeriod("1Y") -> value))
	
  def apply(valuedate:Date, currency:String, values:Map[qlPeriod, Double]):Option[CashCurve] 
		= apply(buildCurve(valuedate, values), currency)
  
  def apply(curve:YieldParameter, currency:String):Option[CashCurve]
		= RateConvention(currency) collect {case conv => CashCurve(curve, conv.iborindex(new qlPeriod(6, TimeUnit.Months)))}
  
}