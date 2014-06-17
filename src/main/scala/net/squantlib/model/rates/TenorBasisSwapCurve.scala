package net.squantlib.model.rates

import net.squantlib.model.yieldparameter.YieldParameter
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.America.USDCurrency
import org.jquantlib.indexes.IborIndex
import org.jquantlib.indexes.ibor.USDLibor
import org.jquantlib.time.{TimeUnit, Period => qlPeriod}
import net.squantlib.model.yieldparameter._
import net.squantlib.util.Date
import net.squantlib.model.rates.convention.RateConvention


/**
 * Tenor basis swap rate curve.
 * Currently restricted to basis 3 months vs 6 months, quoted as 3 months spread against 6 months in same currency.
 * 
 * @constructor stores each information
 * @param daycount and frequency convention (should be quarterly with standard cash daycount)
 */
case class TenorBasisSwapCurve (
    rate:YieldParameter, 
    shortindex:IborIndex, 
    longindex:IborIndex) extends AbstractCurve {

  require(shortindex.tenor().length == 3 && longindex.tenor().length == 6 && shortindex.currency == longindex.currency)
  
  val currency = shortindex.currency
  
  def shifted(shift:(Double, Double) => Double):TenorBasisSwapCurve = new TenorBasisSwapCurve(rate.shifted(shift), shortindex, longindex)
  
}

object TenorBasisSwapCurve{
  
	def defaultCurve(valuedate:Date, values:Map[qlPeriod, Double]):YieldParameter	
		= (values.keySet.size) match {
			case 1 => FlatVector(valuedate, values)
			case 2 => LinearNoExtrapolation(valuedate, values)
			case _ => SplineNoExtrapolation(valuedate, values, 2)
  		} 
	
	def apply(valuedate:Date, currency:String, values:Map[qlPeriod, Double]):Option[TenorBasisSwapCurve]
		= apply(defaultCurve(valuedate, values), currency)
	
	def apply(curve:YieldParameter, currency:String):Option[TenorBasisSwapCurve]
		= RateConvention(currency) collect { case conv => TenorBasisSwapCurve(curve, conv.basis36ShortIndex, conv.basis36LongIndex) }
}

