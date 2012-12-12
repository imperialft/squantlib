package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class EurRateConvention extends RateConvention{
  import org.jquantlib.indexes.Euribor
  import org.jquantlib.currencies.Europe.EURCurrency
  
  	val currency = new EURCurrency
  
	val useRateDiscount = true
	def iborindex(p:Period) = new Euribor(p)
	val swapFloatIndex = new Euribor(new Period(6, TimeUnit.Months))
	val swapFixDaycount = new Thirty360
	val swapFixPeriod = Frequency.Annual

	val useFXdiscount = false
	val swapPointMultiplier = 10000.0
}


