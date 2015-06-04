package net.squantlib.schedule

import net.squantlib.model.rates.DiscountCurve
import org.jquantlib.daycounters._
import net.squantlib.util.Date
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Calendar, BusinessDayConvention, TimeUnit}

case class CalculationPeriod(
    eventDate:Date, 
    startDate:Date, 
    endDate:Date, 
    paymentDate:Date, 
    var daycounter:DayCounter, 
    var nominal:Double) {
  
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
	
  def shifted(shift:Int):CalculationPeriod = CalculationPeriod(eventDate.add(shift), startDate.add(shift), endDate.add(shift), paymentDate.add(shift), daycounter, nominal)
  
  override def toString = eventDate.toString + " " + startDate.toString + " " + endDate.toString + " " + paymentDate.toString + " " + daycounter.toString
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
      nominal:Double = 1.0):CalculationPeriod = {
    
    val eventDate = (if (inarrears) endDate else startDate).advance(fixingCalendar, -notice, TimeUnit.Days)
    val paymentDate = endDate.adjust(paymentCalendar, paymentConvention)
    new CalculationPeriod(eventDate, startDate, endDate, paymentDate, daycount, nominal)
  }
  
  def simpleCashflow(
      paymentDate:Date, 
      paymentCalendar:Calendar,
      paymentConvention:BusinessDayConvention):CalculationPeriod = 
        
    apply(paymentDate, paymentDate, 0, false, new Absolute, paymentCalendar, paymentCalendar, paymentConvention)
  
}
