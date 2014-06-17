package net.squantlib.model.rates.convention

import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._
import org.jquantlib.currencies.Europe.SEKCurrency
import org.jquantlib.indexes.ibor.STIBOR

class SekRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.STIBOR
  import org.jquantlib.currencies.Europe.SEKCurrency
  
  	val currency = new SEKCurrency
  
	val useRateDiscount = true
	def iborindex(p:Period) = new STIBOR(p)
	val swapFloatIndex = new STIBOR(new Period(3, TimeUnit.Months))
	val swapFixDaycount = new Thirty360
	val swapFixPeriod = Frequency.Annual

	val useFXdiscount = true
	val swapPointMultiplier = 1.0
}
