package squantlib.payoff

import squantlib.model.rates.DiscountCurve
import org.jquantlib.daycounters._
import org.jquantlib.time.{Date => qlDate, _}
import org.jquantlib.daycounters.DayCounter

case class CalculationPeriod(
    eventDate:qlDate, 
    startDate:qlDate, 
    endDate:qlDate, 
    paymentDate:qlDate, 
    daycounter:DayCounter) {
  
	def dayCount:Double = daycounter.yearFraction(startDate, endDate)
	
    def isCurrentPeriod(ref:qlDate):Boolean = (ref ge startDate) && (ref lt endDate)
    
    def accrued(ref:qlDate):Double = if (isCurrentPeriod(ref)) daycounter.yearFraction(startDate, ref) else 0.0
    
    def dayCountAfter(ref:qlDate):Double = 
      if (startDate ge ref) dayCount
      else if (isCurrentPeriod(ref)) daycounter.yearFraction(ref, endDate) 
      else 0.0
    
    def zeroCoupon(curve:DiscountCurve):Double = curve(paymentDate)
    
    def coefficient(curve:DiscountCurve):Double = dayCount * zeroCoupon(curve)
    
    def isAbsolute:Boolean = daycounter match {
        case d:Absolute => true
        case _ => false
      }
	
	def shifted(shift:Int):CalculationPeriod = CalculationPeriod(eventDate.add(shift), startDate.add(shift), endDate.add(shift), paymentDate.add(shift), daycounter)
  
	override def toString = eventDate.shortDate.toString + " " + startDate.shortDate.toString + " " + endDate.shortDate.toString + " " + paymentDate.shortDate.toString + " " + daycounter.toString
}

object CalculationPeriod {
  
  def apply(startDate:qlDate, endDate:qlDate, notice:Int, inarrears:Boolean, daycount:DayCounter, calendar:Calendar, paymentConvention:BusinessDayConvention):CalculationPeriod = {
    val eventDate = calendar.advance(if (inarrears) endDate else startDate, -notice, TimeUnit.Days)
	val paymentDate = calendar.adjust(endDate, paymentConvention)
	new CalculationPeriod(eventDate, startDate, endDate, paymentDate, daycount)
  }
  
  def simpleCashflow(paymentDate:qlDate, calendar:Calendar, paymentConvention:BusinessDayConvention):CalculationPeriod = 
    apply(paymentDate, paymentDate, 0, false, new Absolute, calendar, paymentConvention)
  
}
