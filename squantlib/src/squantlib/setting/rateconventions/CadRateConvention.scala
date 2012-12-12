package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class CadRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.Cdor
  import org.jquantlib.currencies.America.CADCurrency
  
  	val currency = new CADCurrency
  
	val useRateDiscount = true
	def iborindex(p:Period) = new Cdor(p)
	val swapFloatIndex = new Cdor(new Period(3, TimeUnit.Months))
	val swapFixDaycount = new Actual365Fixed
	val swapFixPeriod = Frequency.Semiannual

	val useFXdiscount = false
	val swapPointMultiplier = 10000.0
}

