package net.squantlib.model.rates.convention

import net.squantlib.util.ql.{Period, Frequency, TimeUnit}
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.currencies.Asia.JPYCurrency
import net.squantlib.util.ql.indexes.ibor.JPYLibor

class JpyRateConvention extends RateConvention{
  import net.squantlib.util.ql.indexes.ibor.JPYLibor
  import net.squantlib.util.ql.currencies.Asia.JPYCurrency
  
  	val currency = new JPYCurrency
  
	val useRateDiscount = true
	def iborindex(p:Period) = new JPYLibor(p)
	val swapFloatIndex = new JPYLibor(new Period(6, TimeUnit.Months))
	val swapFixDaycount = new Actual365Fixed
	val swapFixPeriod = Frequency.Semiannual
	
	val useFXdiscount = false
	val swapPointMultiplier = 1.0
}

