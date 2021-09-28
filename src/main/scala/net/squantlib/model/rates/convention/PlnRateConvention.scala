package net.squantlib.model.rates.convention

import net.squantlib.util.ql.{Period, Frequency, TimeUnit}
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.currencies.Europe.PLNCurrency
import net.squantlib.util.ql.indexes.ibor.Wibor

class PlnRateConvention extends RateConvention{
  import net.squantlib.util.ql.indexes.ibor.Wibor
  import net.squantlib.util.ql.currencies.Europe.PLNCurrency
  
  	val currency = new PLNCurrency
  
	val useRateDiscount = false
	def iborindex(p:Period) = new Wibor(p)
	val swapFloatIndex = new Wibor(new Period(6, TimeUnit.Months))
	val swapFixDaycount = new Thirty360
	val swapFixPeriod = Frequency.Annual

	val useFXdiscount = true
	val swapPointMultiplier = 1.0
}



