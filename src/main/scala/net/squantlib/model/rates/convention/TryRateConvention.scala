package net.squantlib.model.rates.convention

import org.jquantlib.time.{Period, Frequency}
import org.jquantlib.daycounters.Actual360
import org.jquantlib.indexes.ibor.USDLibor
import org.jquantlib.currencies.Europe.TRYCurrency

class TryRateConvention extends RateConvention{
  import org.jquantlib.currencies.Europe.TRYCurrency
  
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

