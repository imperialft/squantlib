package net.squantlib.model.rates.convention

import net.squantlib.util.ql.time.{Period, Frequency, TimeUnit}
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.currencies.Europe.HUFCurrency
import net.squantlib.util.ql.indexes.ibor.HUFLibor

class HufRateConvention extends RateConvention{
  import net.squantlib.util.ql.indexes.ibor.HUFLibor
  import net.squantlib.util.ql.currencies.Europe.HUFCurrency
  
  	val currency = new HUFCurrency
  
	val useRateDiscount = false
	def iborindex(p:Period) = new HUFLibor(p)
	val swapFloatIndex = new HUFLibor(new Period(6, TimeUnit.Months))
	val swapFixDaycount = new Actual365Fixed
	val swapFixPeriod = Frequency.Annual

	val useFXdiscount = true
	val swapPointMultiplier = 1.0

	override val useNDSdiscount = false
	
}

