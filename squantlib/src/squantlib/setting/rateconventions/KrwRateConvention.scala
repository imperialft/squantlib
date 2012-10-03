package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class KrwRateConvention extends RateConvention{
  import org.jquantlib.currencies.Asia.KRWCurrency
  
  	val currency = new KRWCurrency

  	val useratediscount = false
	def iborindex(p:Period) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null

	val useFXdiscount = true
	val swappoint_multiplier = 1.0
}


