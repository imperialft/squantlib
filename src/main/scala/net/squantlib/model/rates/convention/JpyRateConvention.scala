package net.squantlib.model.rates.convention

import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._
import org.jquantlib.currencies.Asia.JPYCurrency
import org.jquantlib.indexes.ibor.JPYLibor

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
	val swapPointMultiplier = 1.0
}

