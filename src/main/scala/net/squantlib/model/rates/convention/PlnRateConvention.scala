package net.squantlib.model.rates.convention

import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._
import org.jquantlib.currencies.Europe.PLNCurrency
import org.jquantlib.indexes.ibor.Wibor

class PlnRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.Wibor
  import org.jquantlib.currencies.Europe.PLNCurrency
  
  	val currency = new PLNCurrency
  
	val useRateDiscount = false
	def iborindex(p:Period) = new Wibor(p)
	val swapFloatIndex = new Wibor(new Period(6, TimeUnit.Months))
	val swapFixDaycount = new Thirty360
	val swapFixPeriod = Frequency.Annual

	val useFXdiscount = true
	val swapPointMultiplier = 1.0
}



