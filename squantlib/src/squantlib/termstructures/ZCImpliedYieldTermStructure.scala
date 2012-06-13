package squantlib.termstructures

import squantlib.ratecurve._
import squantlib.parameter.TimeVector

import org.jquantlib.time.{ Date => JDate }
import org.jquantlib.termstructures.AbstractYieldTermStructure
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.Calendar

/**
 * Yield termstructure defined from curve & discount termstructure.
 * 
 * @param reference date is either equal or later than discount value date
 */
class ZCImpliedYieldTermStructure(val curve:DiscountableCurve, val discount:DiscountCurve, val cdr:Calendar, val settlement:Int, val referencedate:JDate) 
extends AbstractYieldTermStructure(referencedate)  {

	require (curve.valuedate == discount.valuedate)
	
	val maxDate = discount.zc.maxdate
	
	override def dayCounter = curve.basedaycount
	
	override def calendar = cdr
	
	override def settlementDays = settlement
	
	def /*@DiscountFactor*/ discountImpl(/*@Time*/ t:Double):Double = {
		/* t is relative to the current reference date*/
		val ref = referenceDate
		val /*@Time*/ originaltime = t + dayCounter.yearFraction(curve.valuedate, ref)
		discount.zc.value(originaltime) / discount.zc.value(ref)
	}
	
	def this(c:DiscountableCurve, d:DiscountCurve, cdr:Calendar) = this(c, d, cdr, 0, d.valuedate)

}