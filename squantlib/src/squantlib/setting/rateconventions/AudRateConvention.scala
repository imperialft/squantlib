package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._
import org.jquantlib.indexes.ibor.BBSW
import org.jquantlib.currencies.Oceania.AUDCurrency

class AudRateConvention extends RateConvention{
  
  	val currency = new AUDCurrency
  
	val useratediscount = true
	def iborindex(p:Period) = new BBSW(p)
	val swap_floatindex = new BBSW(new Period(3, TimeUnit.Months))
	val swap_fixdaycount = new Actual365Fixed
	val swap_fixperiod = Frequency.Semiannual

	val useFXdiscount = false
	val swappoint_multiplier = 10000.0
}

