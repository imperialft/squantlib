package net.squantlib.schedule

import net.squantlib.model.rates.DiscountCurve
import org.jquantlib.daycounters._
import net.squantlib.util.Date
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Calendar, BusinessDayConvention, TimeUnit}
import org.jquantlib.time.calendars.NullCalendar

case class CalculationPeriod(
  eventDate:Date,
  startDate:Date,
  endDate:Date,
  paymentDate:Date,
  var daycounter:DayCounter,
  isRedemption: Boolean,
  var nominal:Double
) {
  
  def dayCount:Double = Date.daycount(startDate, endDate, daycounter)
	
  def isCurrentPeriod(ref:Date):Boolean = (ref ge startDate) && (ref lt endDate)
  
  def accrued(ref:Date):Double = if (isCurrentPeriod(ref)) Date.daycount(startDate, ref, daycounter) else 0.0
  
  def dayCountAfter(ref:Date):Double = 
    if (startDate ge ref) dayCount
    else if (isCurrentPeriod(ref)) Date.daycount(ref, endDate, daycounter) 
    else 0.0
  
  def zeroCoupon(curve:DiscountCurve):Double = curve(paymentDate)
  
  def coefficient(curve:DiscountCurve):Double = dayCount * zeroCoupon(curve) * nominal
  
  def isAbsolute:Boolean = daycounter match {
    case d:Absolute => true
    case _ => false
  }
	
  def shifted(shift:Int):CalculationPeriod = CalculationPeriod(eventDate.add(shift), startDate.add(shift), endDate.add(shift), paymentDate.add(shift), daycounter, isRedemption, nominal)
  
  override def toString = eventDate.toString + " " + startDate.toString + " " + endDate.toString + " " + paymentDate.toString + " " + daycounter.toString

  def getRedemptionLeg(nom:Double):CalculationPeriod = CalculationPeriod(eventDate, startDate, endDate, paymentDate, new Absolute, true, nom)
}

object CalculationPeriod {
  
  def apply(
    startDate:Date,
    endDate:Date,
    notice:Int,
    inarrears:Boolean,
    daycount:DayCounter,
    fixingCalendar:Calendar,
    paymentCalendar:Calendar,
    paymentConvention:BusinessDayConvention,
    isRedemption: Boolean,
    nominal:Double = 1.0,
    fixedDayOfMonth:Option[Int] = None,
    fixingOnCalculationEndDate:Boolean = false
  ):CalculationPeriod = {

    val paymentDate = endDate.adjust(paymentCalendar, paymentConvention)

    val calculationDate = if (inarrears) {
      if (fixingOnCalculationEndDate) endDate else paymentDate
    } else startDate
    
    val baseDate = fixedDayOfMonth match {
      case Some(d) =>
        if (d <= calculationDate.dayOfMonth) { // same month
          Date(calculationDate.year, calculationDate.month, d)
        }

        else { // previous month
          val prevmonth = calculationDate.addMonths(-1)
        Date(prevmonth.year, prevmonth.month, d, true)
        }

      case None => calculationDate
	}
    
    val eventDate = baseDate.advance(fixingCalendar, -notice, TimeUnit.Days)

    new CalculationPeriod(eventDate, startDate, endDate, paymentDate, daycount, isRedemption, nominal)
  }
  
  def simpleCashflow(
      paymentDate:Date, 
      paymentCalendar:Calendar,
      paymentConvention:BusinessDayConvention,
      isRedemption: Boolean):CalculationPeriod = 
        
    apply(paymentDate, paymentDate, 0, false, new Absolute, paymentCalendar, paymentCalendar, paymentConvention, isRedemption)
  
}
