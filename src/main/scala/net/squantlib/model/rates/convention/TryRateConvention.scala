package net.squantlib.model.rates.convention

import net.squantlib.util.ql.time.{Period, Frequency}
import net.squantlib.util.ql.daycounters.Actual360
import net.squantlib.util.ql.indexes.ibor.USDLibor
import net.squantlib.util.ql.currencies.Europe.TRYCurrency

class TryRateConvention extends RateConvention{
  import net.squantlib.util.ql.currencies.Europe.TRYCurrency
  
  	val currency = new TRYCurrency
  	
	val useRateDiscount = false
	def iborindex(p:Period) = null
	val swapFloatIndex = null
	val swapFixDaycount = null
	val swapFixPeriod = null

	val useFXdiscount = true
	val swapPointMultiplier = 1.0
	
	override val useNDSdiscount = false
	override val ndsFixDaycount = new Actual360
	override val ndsFixPeriod = Frequency.Annual
	override val ndsFloatIndex = new USDLibor(new Period("3M"))
	
}

