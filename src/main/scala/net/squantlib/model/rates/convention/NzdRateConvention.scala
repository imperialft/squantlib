package net.squantlib.model.rates.convention

import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._
import org.jquantlib.indexes.ibor.BKBM
import org.jquantlib.currencies.Oceania.NZDCurrency

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

