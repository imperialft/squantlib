package squantlib.model.rates.convention

import squantlib.model.rates.convention.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._
import org.jquantlib.currencies.Europe.HUFCurrency
import org.jquantlib.indexes.ibor.HUFLibor

class HufRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.HUFLibor
  import org.jquantlib.currencies.Europe.HUFCurrency
  
  	val currency = new HUFCurrency
  
	val useRateDiscount = false
	def iborindex(p:Period) = new HUFLibor(p)
	val swapFloatIndex = new HUFLibor(new Period(6, TimeUnit.Months))
	val swapFixDaycount = new Actual365Fixed
	val swapFixPeriod = Frequency.Annual

	val useFXdiscount = true
	val swapPointMultiplier = 100.0

	override val useNDSdiscount = false
	
}

