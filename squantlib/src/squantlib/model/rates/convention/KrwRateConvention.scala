package squantlib.model.rates.convention

import squantlib.model.rates.convention.RateConvention
import org.jquantlib.time.Period
import org.jquantlib.daycounters._
import org.jquantlib.currencies.Asia.KRWCurrency

class KrwRateConvention extends RateConvention{
  import org.jquantlib.currencies.Asia.KRWCurrency
  
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


