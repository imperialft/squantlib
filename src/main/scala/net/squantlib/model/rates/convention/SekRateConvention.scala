package net.squantlib.model.rates.convention

import net.squantlib.util.ql.{Period, Frequency, TimeUnit}
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.currencies.Europe.SEKCurrency
import net.squantlib.util.ql.indexes.ibor.STIBOR

class SekRateConvention extends RateConvention{
  import net.squantlib.util.ql.indexes.ibor.STIBOR
  import net.squantlib.util.ql.currencies.Europe.SEKCurrency
  
  	val currency = new SEKCurrency
  
	val useRateDiscount = true
	def iborindex(p:Period) = new STIBOR(p)
	val swapFloatIndex = new STIBOR(new Period(3, TimeUnit.Months))
	val swapFixDaycount = new Thirty360
	val swapFixPeriod = Frequency.Annual

	val useFXdiscount = true
	val swapPointMultiplier = 1.0
}
