package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters.Actual360
import org.jquantlib.indexes.ibor.USDLibor

class TryRateConvention extends RateConvention{
  import org.jquantlib.currencies.Europe.TRYCurrency
  
  	val currency = new TRYCurrency
  	
	val useRateDiscount = false
	def iborindex(p:Period) = null
	val swapFloatIndex = null
	val swapFixDaycount = null
	val swapFixPeriod = null

	val useFXdiscount = true
	val swapPointMultiplier = 10000.0
	
	override val useNDSdiscount = false
	override val ndsFixDaycount = new Actual360
	override val ndsFixPeriod = Frequency.Annual
	override val ndsFloatIndex = new USDLibor(new Period("3M"))
	
}

