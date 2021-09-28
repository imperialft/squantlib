package net.squantlib.model.rates.convention

import net.squantlib.util.ql.time.Period
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.currencies.America.BRLCurrency
import net.squantlib.util.ql.daycounters.Actual360
  
class BrlRateConvention extends RateConvention{
  
  val currency = new BRLCurrency
  	
	val useRateDiscount = false
	def iborindex(p:Period) = null
	val swapFloatIndex = null
	val swapFixDaycount = null
	val swapFixPeriod = null

	val useFXdiscount = true
	val swapPointMultiplier = 1.0	
	
	override val useNDSdiscount = false
	override val ndsFixDaycount = new Actual360
	
}
