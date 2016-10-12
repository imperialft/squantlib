package net.squantlib.model.rates.convention

import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._
import org.jquantlib.indexes.ibor.BBSW
import org.jquantlib.currencies.Oceania.AUDCurrency

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

