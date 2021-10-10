package net.squantlib.model.rates.convention

import net.squantlib.util.ql.time.{Period, Frequency, TimeUnit}
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.indexes.ibor.BKBM
import net.squantlib.util.ql.currencies.Oceania.NZDCurrency

class NzdRateConvention extends RateConvention{
  
  	val currency = new NZDCurrency
  
	val useRateDiscount = true
	def iborindex(p:Period) = new BKBM(p)
	val swapFloatIndex = new BKBM(new Period(3, TimeUnit.Months))
	val swapFixDaycount = new Actual365Fixed
	val swapFixPeriod = Frequency.Semiannual

	val useFXdiscount = false
	val swapPointMultiplier = 1.0
}

