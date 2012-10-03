package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class InrRateConvention extends RateConvention{
  import org.jquantlib.currencies.Asia.INRCurrency
  
  	val currency = new INRCurrency

  	val useratediscount = false
	def iborindex(p:Period) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null

	val useFXdiscount = true
	val swappoint_multiplier = 100.0
}
