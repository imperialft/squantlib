package net.squantlib.model.rates.convention

import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._
import org.jquantlib.currencies.America.CADCurrency
import org.jquantlib.indexes.ibor.Cdor

class CadRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.Cdor
  import org.jquantlib.currencies.America.CADCurrency
  
  	val currency = new CADCurrency
  
	val useRateDiscount = false
	def iborindex(p:Period) = new Cdor(p)
	val swapFloatIndex = new Cdor(new Period(3, TimeUnit.Months))
	val swapFixDaycount = new Actual365Fixed
	val swapFixPeriod = Frequency.Semiannual

	val useFXdiscount = true
	val swapPointMultiplier = 1.0
}

