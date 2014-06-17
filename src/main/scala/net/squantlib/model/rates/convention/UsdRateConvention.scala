package net.squantlib.model.rates.convention


import org.jquantlib.time.{Period, Frequency, TimeUnit}
import org.jquantlib.daycounters._
import org.jquantlib.currencies.America.USDCurrency
import org.jquantlib.indexes.ibor.USDLibor

class UsdRateConvention extends RateConvention{
  import org.jquantlib.indexes.ibor.USDLibor
  import org.jquantlib.currencies.America.USDCurrency
  
  	val currency = new USDCurrency
  	
	val useRateDiscount = true
	def iborindex(p:Period) = new USDLibor(p)
	val swapFloatIndex = new USDLibor(new Period(3, TimeUnit.Months))
	val swapFixDaycount = new Actual360
	val swapFixPeriod = Frequency.Annual
	
	val useFXdiscount = false
	val swapPointMultiplier = -99999.0
}


