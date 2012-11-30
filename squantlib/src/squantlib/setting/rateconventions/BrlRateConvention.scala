package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._
import org.jquantlib.currencies.America.BRLCurrency
  
class BrlRateConvention extends RateConvention{
  
  	val currency = new BRLCurrency
  	
	val useratediscount = false
	def iborindex(p:Period) = null
	val swap_floatindex = null
	val swap_fixdaycount = null
	val swap_fixperiod = null

	val useFXdiscount = true
	val swappoint_multiplier = 10000.0
}
