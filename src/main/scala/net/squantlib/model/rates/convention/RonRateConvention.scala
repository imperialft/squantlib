package net.squantlib.model.rates.convention

import net.squantlib.util.ql.time.Period
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.currencies.Europe.RONCurrency

class RonRateConvention extends RateConvention{
  import net.squantlib.util.ql.currencies.Europe.RONCurrency
  
  	val currency = new RONCurrency

  	val useRateDiscount = false
	def iborindex(p:Period) = null
	val swapFloatIndex = null
	val swapFixDaycount = null
	val swapFixPeriod = null

	val useFXdiscount = true
	val swapPointMultiplier = 1.0
}

