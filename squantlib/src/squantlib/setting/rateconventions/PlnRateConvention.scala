package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class PlnRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.Wibor
  import org.jquantlib.currencies.Europe.PLNCurrency
  
  	val currency = new PLNCurrency
  
	val useRateDiscount = false
	def iborindex(p:Period) = new Wibor(p)
	val swapFloatIndex = new Wibor(new Period(6, TimeUnit.Months))
	val swapFixDaycount = new Thirty360
	val swapFixPeriod = Frequency.Annual

	val useFXdiscount = true
	val swapPointMultiplier = 10000.0
}



