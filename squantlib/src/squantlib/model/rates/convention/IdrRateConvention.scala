package squantlib.model.rates.convention

import squantlib.model.rates.convention.RateConvention
import org.jquantlib.time.Period
import org.jquantlib.daycounters._
import org.jquantlib.currencies.Asia.IDRCurrency

class IdrRateConvention extends RateConvention{
  import org.jquantlib.currencies.Asia.IDRCurrency
  
  	val currency = new IDRCurrency
  	
	val useRateDiscount = false
	def iborindex(p:Period) = null
	val swapFloatIndex = null
	val swapFixDaycount = null
	val swapFixPeriod = null

	val useFXdiscount = false
	val swapPointMultiplier = 1.0
	
	override val useNDSdiscount = true
	override val ndsFixDaycount = new Actual365Fixed
	
}



