package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class HufRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.HUFLibor
  import org.jquantlib.currencies.Europe.HUFCurrency
  
  	val currency = new HUFCurrency
  
	val useratediscount = false
	def iborindex(p:Period) = new HUFLibor(p)
	val swap_floatindex = new HUFLibor(new Period(6, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Annual

	val useFXdiscount = true
	val swappoint_multiplier = 100.0
}

