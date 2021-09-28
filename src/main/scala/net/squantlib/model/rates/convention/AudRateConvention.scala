package net.squantlib.model.rates.convention

import net.squantlib.util.ql.time.{Period, Frequency, TimeUnit}
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.indexes.ibor.BBSW
import net.squantlib.util.ql.currencies.Oceania.AUDCurrency

class AudRateConvention extends RateConvention{
  
  	val currency = new AUDCurrency
  
	val useRateDiscount = true
	def iborindex(p:Period) = new BBSW(p)
	val swapFloatIndex = new BBSW(new Period(3, TimeUnit.Months))
	val swapFixDaycount = new Actual365Fixed
	val swapFixPeriod = Frequency.Semiannual

	val useFXdiscount = false
	val swapPointMultiplier = 1.0
}

