package net.squantlib.model.rates.convention

import net.squantlib.util.ql.{Period, Frequency, TimeUnit}
import net.squantlib.util.ql.daycounters._

class CadRateConvention extends RateConvention{
  import net.squantlib.util.ql.indexes.ibor.Cdor
  import net.squantlib.util.ql.currencies.America.CADCurrency
  
	val currency = new CADCurrency
  
	val useRateDiscount = false
	def iborindex(p:Period) = new Cdor(p)
	val swapFloatIndex = new Cdor(new Period(3, TimeUnit.Months))
	val swapFixDaycount = new Actual365Fixed
	val swapFixPeriod = Frequency.Semiannual

	val useFXdiscount = true
	val swapPointMultiplier = 1.0
}

