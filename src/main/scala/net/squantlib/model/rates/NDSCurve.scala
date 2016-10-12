package net.squantlib.model.rates

import net.squantlib.model.yieldparameter._
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.America.USDCurrency
import org.jquantlib.indexes.IborIndex
import org.jquantlib.indexes.ibor.USDLibor
import org.jquantlib.time.{TimeUnit, Period => qlPeriod, Frequency}
import net.squantlib.model.rates.convention.RateConvention
import net.squantlib.util.Date

/**
 * NDS rate curve
 * 
 * @constructor stores each information
 * @param float index, daycount & payment frequency for fixed leg
 */
case class NDSCurve (rate:YieldParameter, currency:Currency, fixdaycount:DayCounter, fixperiod:Frequency, floatindex:IborIndex) extends AbstractCurve {
  require (floatindex.tenor.units == TimeUnit.Months && List(3, 6).contains(floatindex.tenor.length))
  
  val floatCurrency = floatindex.currency
  
  def shifted(shift:(Double, Double) => Double):NDSCurve = NDSCurve(rate.shifted(shift), currency, fixdaycount, fixperiod, floatindex)
  
}


object NDSCurve {
  
  def buildCurve(valuedate:Date, values:Map[qlPeriod, Double]):YieldParameter = (values.keySet.size) match {
    case 1 => FlatVector(valuedate, values)
	case 2 => LinearNoExtrapolation(valuedate, values)
	case _ => SplineNoExtrapolation(valuedate, values, 2) 
	} 
	
  def apply(valuedate:Date, currency:String, value:Double):Option[NDSCurve] 
    = apply(valuedate, currency, Map(new qlPeriod("1Y") -> value))
	
  def apply(valuedate:Date, currency:String, values:Map[qlPeriod, Double]):Option[NDSCurve] 
    = apply(buildCurve(valuedate, values), currency)
  
  def apply(curve:YieldParameter, currency:String):Option[NDSCurve]
    = RateConvention(currency) collect {case conv => NDSCurve(curve, conv.currency, conv.ndsFixDaycount, conv.ndsFixPeriod, conv.ndsFloatIndex)}
  
}