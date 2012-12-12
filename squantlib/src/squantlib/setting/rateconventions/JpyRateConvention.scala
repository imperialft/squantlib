package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class JpyRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.JPYLibor
  import org.jquantlib.currencies.Asia.JPYCurrency
  
  	val currency = new JPYCurrency
  
	val useRateDiscount = true
	def iborindex(p:Period) = new JPYLibor(p)
	val swapFloatIndex = new JPYLibor(new Period(6, TimeUnit.Months))
	val swapFixDaycount = new Actual365Fixed
	val swapFixPeriod = Frequency.Semiannual
	
	val useFXdiscount = false
	val swapPointMultiplier = 100.0
}

