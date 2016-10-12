package net.squantlib.model.rates.convention

import org.jquantlib.time.Period
import org.jquantlib.daycounters._
import org.jquantlib.currencies.America.MXNCurrency

class MxnRateConvention extends RateConvention{
  import org.jquantlib.currencies.America.MXNCurrency
  
  	val currency = new MXNCurrency
  
	val useRateDiscount = false
	def iborindex(p:Period) = null
	val swapFloatIndex = null
	val swapFixDaycount = null
	val swapFixPeriod = null

	val useFXdiscount = true
	val swapPointMultiplier = 1.0
}

