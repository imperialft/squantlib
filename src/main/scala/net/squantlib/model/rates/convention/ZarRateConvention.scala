package net.squantlib.model.rates.convention


import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._
import org.jquantlib.currencies.Africa.ZARCurrency
import org.jquantlib.indexes.ibor.Jibar

class ZarRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.Jibar
  import org.jquantlib.currencies.Africa.ZARCurrency
  
  	val currency = new ZARCurrency
  
	val useRateDiscount = true
	def iborindex(p:Period) = new Jibar(p)
	val swapFloatIndex = new Jibar(new Period(3, TimeUnit.Months))
	val swapFixDaycount = new Actual365Fixed
	val swapFixPeriod = Frequency.Quarterly

	val useFXdiscount = false
	val swapPointMultiplier = 1.0
}
