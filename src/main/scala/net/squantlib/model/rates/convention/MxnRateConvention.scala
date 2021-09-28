package net.squantlib.model.rates.convention

import net.squantlib.util.ql.Period
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.currencies.America.MXNCurrency

class MxnRateConvention extends RateConvention{
  import net.squantlib.util.ql.currencies.America.MXNCurrency
  
  	val currency = new MXNCurrency
  
	val useRateDiscount = false
	def iborindex(p:Period) = null
	val swapFloatIndex = null
	val swapFixDaycount = null
	val swapFixPeriod = null

	val useFXdiscount = true
	val swapPointMultiplier = 1.0
}

