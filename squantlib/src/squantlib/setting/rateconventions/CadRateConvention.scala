package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class CadRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.Cdor
  import org.jquantlib.currencies.America.CADCurrency
  
  	val currency = new CADCurrency
  
	val useratediscount = true
	def iborindex(p:Period) = new Cdor(p)
	val swap_floatindex = new Cdor(new Period(3, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Semiannual

	val useFXdiscount = false
	val swappoint_multiplier = 10000.0
}

