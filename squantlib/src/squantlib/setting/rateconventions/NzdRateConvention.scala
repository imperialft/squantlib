package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class NzdRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.BKBM
  import org.jquantlib.currencies.Oceania.NZDCurrency
  
  	val currency = new NZDCurrency
  
	val useratediscount = true
	def iborindex(p:Period) = new BKBM(p)
	val swap_floatindex = new BKBM(new Period(3, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Semiannual

	val useFXdiscount = false
	val swappoint_multiplier = 10000.0
}

