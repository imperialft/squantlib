package squantlib.model.rates

import squantlib.model.yieldparameter.YieldParameter
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.currencies.Currency
import org.jquantlib.currencies.America.USDCurrency
import org.jquantlib.indexes.IborIndex
import org.jquantlib.indexes.ibor.USDLibor
import org.jquantlib.time.{TimeUnit, Date => JDate, Period => JPeriod, Frequency }


/**
 * NDS rate curve
 * 
 * @constructor stores each information
 * @param float index, daycount & payment frequency for fixed leg
 */
case class NDSCurve (rate:YieldParameter, currency:Currency, floatindex:IborIndex, fixdaycount:DayCounter, fixperiod:Frequency) extends AbstractCurve{
  require (floatindex.tenor().units() == TimeUnit.Months && List(3, 6).contains(floatindex.tenor().length()))
  val floatCurrency = floatindex.currency
  def shifted(shift:(Double, Double) => Double):SwapCurve = new SwapCurve(rate.shifted(shift), floatindex, fixdaycount, fixperiod)
}
