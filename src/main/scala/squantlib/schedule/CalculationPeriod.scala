package squantlib.schedule

import squantlib.model.rates.DiscountCurve
import org.jquantlib.daycounters._
import squantlib.util.Date
import org.jquantlib.time.{Date => jDate, _}
import org.jquantlib.daycounters.DayCounter

case class CalculationPeriod(
    eventDate:Date, 
    startDate:Date, 
    endDate:Date, 
    paymentDate:Date, 
    daycounter:DayCounter) {
  
	def dayCount:Double = Date.daycount(startDate, endDate, daycounter)
	
    def isCurrentPeriod(ref:Date):Boolean = (ref ge startDate) && (ref lt endDate)
    
    def accrued(ref:Date):Double = if (isCurrentPeriod(ref)) Date.daycount(startDate, ref, daycounter) else 0.0
    
    def dayCountAfter(ref:Date):Double = 
      if (startDate ge ref) dayCount
      else if (isCurrentPeriod(ref)) Date.daycount(ref, endDate, daycounter) 
      else 0.0
    
    def zeroCoupon(curve:DiscountCurve):Double = curve(paymentDate)
    
    def coefficient(curve:DiscountCurve):Double = dayCount * zeroCoupon(curve)
    
    def isAbsolute:Boolean = daycounter match {
        case d:Absolute => true
        case _ => false
      }
	
	def shifted(shift:Int):CalculationPeriod = CalculationPeriod(eventDate.add(shift), startDate.add(shift), endDate.add(shift), paymentDate.add(shift), daycounter)
  
	override def toString = eventDate.toString + " " + startDate.toString + " " + endDate.toString + " " + paymentDate.toString + " " + daycounter.toString
}

object CalculationPeriod {
  
  def apply(startDate:Date, endDate:Date, notice:Int, inarrears:Boolean, daycount:DayCounter, calendar:Calendar, paymentConvention:BusinessDayConvention):CalculationPeriod = {
    val eventDate = (if (inarrears) endDate else startDate).advance(calendar, -notice, TimeUnit.Days)
	val paymentDate = endDate.adjust(calendar, paymentConvention)
	new CalculationPeriod(eventDate, startDate, endDate, paymentDate, daycount)
  }
  
  def simpleCashflow(paymentDate:Date, calendar:Calendar, paymentConvention:BusinessDayConvention):CalculationPeriod = 
    apply(paymentDate, paymentDate, 0, false, new Absolute, calendar, paymentConvention)
  
}
