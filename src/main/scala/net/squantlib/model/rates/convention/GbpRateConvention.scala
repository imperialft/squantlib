package net.squantlib.model.rates.convention

import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._
import org.jquantlib.currencies.Europe.GBPCurrency
import org.jquantlib.indexes.ibor.GBPLibor

class GbpRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.GBPLibor
  import org.jquantlib.currencies.Europe.GBPCurrency
  
  	val currency = new GBPCurrency
  
	val useRateDiscount = true
	def iborindex(p:Period) = new GBPLibor(p)
	val swapFloatIndex = new GBPLibor(new Period(6, TimeUnit.Months))
	val swapFixDaycount = new Actual365Fixed
	val swapFixPeriod = Frequency.Semiannual

	val useFXdiscount = false
	val swapPointMultiplier = 1.0
}


