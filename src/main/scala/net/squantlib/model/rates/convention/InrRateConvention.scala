package net.squantlib.model.rates.convention

import net.squantlib.util.ql.Period
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.currencies.Asia.INRCurrency

class InrRateConvention extends RateConvention{
  import net.squantlib.util.ql.currencies.Asia.INRCurrency
  
  	val currency = new INRCurrency

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
