package squantlib.termstructures

import squantlib.model.discountcurve.{DiscountableCurve, DiscountCurve}
import squantlib.parameter.yieldparameter.YieldParameter
import squantlib.model.currencies.CurrencyConversion
import org.jquantlib.time.{ Date => JDate, Calendar}
import org.jquantlib.termstructures.AbstractYieldTermStructure
import org.jquantlib.daycounters.DayCounter


/**
 * Yield termstructure defined from curve & discount termstructure.
 * 
 * @param reference date is either equal or later than discount value date
 */
class ZCImpliedYieldTermStructure(val discount:DiscountCurve, val cdr:Calendar, val settlement:Int, val referencedate:JDate) 
extends AbstractYieldTermStructure(referencedate)  {

	val maxDate = discount.zc.maxdate
	
	override def dayCounter = discount.daycount
	
	override def calendar = cdr
	
	override def settlementDays = settlement
	
	def /*@DiscountFactor*/ discountImpl(/*@Time*/ t:Double):Double = {
		/* t is relative to the current reference date*/
		val ref = referenceDate
		val /*@Time*/ originaltime = t + dayCounter.yearFraction(discount.valuedate, ref)
		discount.zc.value(originaltime, dayCounter) / discount.zc.value(ref)
	}
	
	def this(d:DiscountCurve, cdr:Calendar) = this(d, cdr, 0, d.valuedate)
	def this(d:DiscountCurve) = this(d, CurrencyConversion.getcalendar(d.currency), 0, d.valuedate)

}