package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class PlnRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.Wibor
  import org.jquantlib.currencies.Europe.PLNCurrency
  
  	val currency = new PLNCurrency
  
	val useratediscount = false
	def iborindex(p:Period) = new Wibor(p)
	val swap_floatindex = new Wibor(new Period(6, TimeUnit.Months))
	val swap_fixdaycount = new Thirty360
	val swap_fixperiod = Frequency.Annual

	val useFXdiscount = true
	val swappoint_multiplier = 10000.0
}



