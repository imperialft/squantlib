package net.squantlib.model.rates.convention

import org.jquantlib.time.Period
import org.jquantlib.daycounters._
import org.jquantlib.currencies.Europe.RONCurrency

class RonRateConvention extends RateConvention{
  import org.jquantlib.currencies.Europe.RONCurrency
  
  	val currency = new RONCurrency

  	val useRateDiscount = false
	def iborindex(p:Period) = null
	val swapFloatIndex = null
	val swapFixDaycount = null
	val swapFixPeriod = null

	val useFXdiscount = true
	val swapPointMultiplier = 1.0
}

