package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class SekRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.STIBOR
  import org.jquantlib.currencies.Europe.SEKCurrency
  
  	val currency = new SEKCurrency
  
	val useratediscount = true
	def iborindex(p:Period) = new STIBOR(p)
	val swap_floatindex = new STIBOR(new Period(3, TimeUnit.Months))
	val swap_fixdaycount = new Thirty360
	val swap_fixperiod = Frequency.Annual

	val useFXdiscount = true
	val swappoint_multiplier = 10000.0
}
