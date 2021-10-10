package net.squantlib.model.rates.convention

import net.squantlib.util.ql.time.{Frequency, Period, TimeUnit}
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.currencies.Europe.EURCurrency
import net.squantlib.util.ql.indexes.Euribor

class EurRateConvention extends RateConvention{
  import net.squantlib.util.ql.indexes.Euribor
  import net.squantlib.util.ql.currencies.Europe.EURCurrency
  
  	val currency = new EURCurrency
  
	val useRateDiscount = true
	def iborindex(p:Period) = new Euribor(p)
	val swapFloatIndex = new Euribor(new Period(6, TimeUnit.Months))
	val swapFixDaycount = new Thirty360
	val swapFixPeriod = Frequency.Annual

	val useFXdiscount = false
	val swapPointMultiplier = 1.0
}


