package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class EurRateConvention extends RateConvention{
  import org.jquantlib.indexes.Euribor
  import org.jquantlib.currencies.Europe.EURCurrency
  
  	val currency = new EURCurrency
  
	val useratediscount = true
	def iborindex(p:Period) = new Euribor(p)
	val swap_floatindex = new Euribor(new Period(6, TimeUnit.Months))
	val swap_fixdaycount = new Thirty360
	val swap_fixperiod = Frequency.Annual

	val useFXdiscount = false
	val swappoint_multiplier = 10000.0
}


