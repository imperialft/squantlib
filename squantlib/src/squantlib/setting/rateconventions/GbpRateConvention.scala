package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class GbpRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.GBPLibor
  import org.jquantlib.currencies.Europe.GBPCurrency
  
  	val currency = new GBPCurrency
  
	val useratediscount = true
	def iborindex(p:Period) = new GBPLibor(p)
	val swap_floatindex = new GBPLibor(new Period(6, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Semiannual

	val useFXdiscount = false
	val swappoint_multiplier = 10000.0
}


