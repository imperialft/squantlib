package net.squantlib.model.rates.convention


import net.squantlib.util.ql.time.{Period, Frequency, TimeUnit}
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.currencies.America.USDCurrency
import net.squantlib.util.ql.indexes.ibor.USDLibor

class UsdRateConvention extends RateConvention{
  import net.squantlib.util.ql.indexes.ibor.USDLibor
  import net.squantlib.util.ql.currencies.America.USDCurrency
  
  	val currency = new USDCurrency
  	
	val useRateDiscount = true
	def iborindex(p:Period) = new USDLibor(p)
	val swapFloatIndex = new USDLibor(new Period(3, TimeUnit.Months))
	val swapFixDaycount = new Actual360
	val swapFixPeriod = Frequency.Annual
	
	val useFXdiscount = false
	val swapPointMultiplier = -99999.0
}


