package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class ZarRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.Jibar
  import org.jquantlib.currencies.Africa.ZARCurrency
  
  	val currency = new ZARCurrency
  
	val useratediscount = true
	def iborindex(p:Period) = new Jibar(p)
	val swap_floatindex = new Jibar(new Period(3, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Quarterly

	val useFXdiscount = false
	val swappoint_multiplier = 10000.0
}
