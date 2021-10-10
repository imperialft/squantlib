package net.squantlib.model.rates.convention

import net.squantlib.util.ql.time.Period
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.currencies.Asia.KRWCurrency

class KrwRateConvention extends RateConvention{
  import net.squantlib.util.ql.currencies.Asia.KRWCurrency
  
  	val currency = new KRWCurrency

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


