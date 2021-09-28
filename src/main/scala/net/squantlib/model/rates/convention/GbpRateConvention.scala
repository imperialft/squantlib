package net.squantlib.model.rates.convention

import net.squantlib.util.ql.{Period, Frequency, TimeUnit}
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.currencies.Europe.GBPCurrency
import net.squantlib.util.ql.indexes.ibor.GBPLibor

class GbpRateConvention extends RateConvention{
  import net.squantlib.util.ql.indexes.ibor.GBPLibor
  import net.squantlib.util.ql.currencies.Europe.GBPCurrency
  
  	val currency = new GBPCurrency
  
	val useRateDiscount = true
	def iborindex(p:Period) = new GBPLibor(p)
	val swapFloatIndex = new GBPLibor(new Period(6, TimeUnit.Months))
	val swapFixDaycount = new Actual365Fixed
	val swapFixPeriod = Frequency.Semiannual

	val useFXdiscount = false
	val swapPointMultiplier = 1.0
}


