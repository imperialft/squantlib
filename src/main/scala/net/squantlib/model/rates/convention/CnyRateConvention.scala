package net.squantlib.model.rates.convention

import net.squantlib.util.ql.Period
import net.squantlib.util.ql.daycounters.Actual365Fixed

class CnyRateConvention extends RateConvention{
  import net.squantlib.util.ql.currencies.Asia.CNYCurrency
  
  	val currency = new CNYCurrency

  	val useRateDiscount = false
	def iborindex(p:Period) = null
	val swapFloatIndex = null
	val swapFixDaycount = null
	val swapFixPeriod = null

	val useFXdiscount = true
	val swapPointMultiplier = 1.0

	override val useNDSdiscount = false
	override val ndsFixDaycount = new Actual365Fixed
}


