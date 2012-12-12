package squantlib.setting.rateconventions

import squantlib.setting.RateConvention
import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._

class InrRateConvention extends RateConvention{
  import org.jquantlib.currencies.Asia.INRCurrency
  
  	val currency = new INRCurrency

  	val useRateDiscount = false
	def iborindex(p:Period) = null
	val swapFloatIndex = null
	val swapFixDaycount = null
	val swapFixPeriod = null

	val useFXdiscount = true
	val swapPointMultiplier = 100.0

	override val useNDSdiscount = false
	override val ndsFixDaycount = new Actual365Fixed

}
