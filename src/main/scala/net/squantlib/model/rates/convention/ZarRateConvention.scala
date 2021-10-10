package net.squantlib.model.rates.convention


import net.squantlib.util.ql.time.{Period, Frequency, TimeUnit}
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.currencies.Africa.ZARCurrency
import net.squantlib.util.ql.indexes.ibor.Jibar

class ZarRateConvention extends RateConvention{
  import net.squantlib.util.ql.indexes.ibor.Jibar
  import net.squantlib.util.ql.currencies.Africa.ZARCurrency
  
  	val currency = new ZARCurrency
  
	val useRateDiscount = true
	def iborindex(p:Period) = new Jibar(p)
	val swapFloatIndex = new Jibar(new Period(3, TimeUnit.Months))
	val swapFixDaycount = new Actual365Fixed
	val swapFixPeriod = Frequency.Quarterly

	val useFXdiscount = false
	val swapPointMultiplier = 1.0
}
