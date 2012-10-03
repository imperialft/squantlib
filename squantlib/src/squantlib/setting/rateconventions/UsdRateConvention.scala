package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class UsdRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.USDLibor
  import org.jquantlib.currencies.America.USDCurrency
  
  	val currency = new USDCurrency
  	
	val useratediscount = true
	def iborindex(p:Period) = new USDLibor(p)
	val swap_floatindex = new USDLibor(new Period(3, TimeUnit.Months))
	val swap_fixdaycount = new Actual360
	val swap_fixperiod = Frequency.Annual
	
	val useFXdiscount = false
	val swappoint_multiplier = -99999.0
}


