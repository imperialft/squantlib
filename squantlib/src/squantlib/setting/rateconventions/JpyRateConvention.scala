package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class JpyRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.JPYLibor
  import org.jquantlib.currencies.Asia.JPYCurrency
  
  	val currency = new JPYCurrency
  
	val useratediscount = true
	def iborindex(p:Period) = new JPYLibor(p)
	val swap_floatindex = new JPYLibor(new Period(6, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Semiannual
	
	val useFXdiscount = false
	val swappoint_multiplier = 100.0
}

