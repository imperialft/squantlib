package squantlib.model.rates

import squantlib.model.yieldparameter._
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.currencies.Currency
import org.jquantlib.indexes.IborIndex
import org.jquantlib.time.{TimeUnit, Date => qlDate, Period => qlPeriod, Frequency }
import squantlib.setting.RateConvention

/**
 * Swap rate curve
 * 
 * @constructor stores each information
 * @param float index, daycount & payment frequency for fixed leg
 */
case class SwapCurve (rate:YieldParameter, floatindex:IborIndex, fixdaycount:DayCounter, fixperiod:Frequency) extends AbstractCurve{
  require (floatindex.tenor().units() == TimeUnit.Months && List(3, 6).contains(floatindex.tenor().length()))
  val currency = floatindex.currency
  def shifted(shift:(Double, Double) => Double):SwapCurve = new SwapCurve(rate.shifted(shift), floatindex, fixdaycount, fixperiod)
}

object SwapCurve {
  
  def swap_curve(valuedate:qlDate, values:Map[qlPeriod, Double]):YieldParameter
	= (values.keySet.size) match {
		case 1 => new FlatVector(valuedate, values)
		case 2 => new LinearNoExtrapolation(valuedate, values)
		case _ => new SplineNoExtrapolation(valuedate, values, 2)} 

  /**
   * Returns swap curve using specified conventions and curve construction method.
   */
  def apply(valuedate:qlDate, currency:String, values:Map[qlPeriod, Double]):Option[SwapCurve]
	= apply(swap_curve(valuedate, values), currency)
	
  def apply(curve:YieldParameter, currency:String):Option[SwapCurve]
	= RateConvention(currency) collect { case conv => SwapCurve(curve, conv.swapFloatIndex, conv.swapFixDaycount, conv.swapFixPeriod)}

}

