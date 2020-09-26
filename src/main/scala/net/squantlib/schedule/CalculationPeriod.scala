package net.squantlib.schedule

import net.squantlib.model.rates.DiscountCurve
import org.jquantlib.daycounters._
import net.squantlib.util.Date
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.{Calendar, BusinessDayConvention, TimeUnit}
import org.jquantlib.time.calendars.NullCalendar

case class CalculationPeriod(
  eventDate:Date,
  callEventDate:Date,
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
	
  def shifted(shift:Int):CalculationPeriod = {
    CalculationPeriod(
      eventDate = eventDate.add(shift),
      callEventDate = callEventDate.add(shift),
      startDate = startDate.add(shift),
      endDate = endDate.add(shift),
      paymentDate = paymentDate.add(shift),
      daycounter = daycounter,
      isRedemption = isRedemption,
      nominal = nominal
    )
  }
  
  override def toString = {
    eventDate.toString + " " + startDate.toString + " " + endDate.toString + " " + paymentDate.toString + " " + daycounter.toString
  }

  def getRedemptionLeg(nom:Double):CalculationPeriod = {
    CalculationPeriod(
      eventDate = eventDate,
      callEventDate = callEventDate,
      startDate = endDate,
      endDate = endDate,
      paymentDate = paymentDate,
      daycounter = new Absolute,
      isRedemption = true,
      nominal = nom
    )
  }

}

object CalculationPeriod {
  
  def apply(
    startDate:Date,
    endDate:Date,
    couponNotice:Int,
    callNotice:Option[Int],
    inArrears:Boolean,
    daycounter:DayCounter,
    fixingCalendar:Calendar,
    fixingAdjustmentCalendar:Option[Calendar],
    fixingAdjustmentConvention:BusinessDayConvention,
    paymentCalendar:Calendar,
    paymentConvention:BusinessDayConvention,
    isRedemption: Boolean,
    nominal:Double = 1.0,
    fixedDayOfMonth:Option[Int] = None,
    fixingOnCalculationEndDate:Boolean = false
  ):CalculationPeriod = {

    val paymentDate = endDate.adjust(paymentCalendar, paymentConvention)

    val calculationDate = {
      if (inArrears) {
        if (fixingOnCalculationEndDate) endDate
        else paymentDate
      }
      else startDate
    }
    
    val baseDate = fixedDayOfMonth match {
      case Some(d) =>
        if (d <= calculationDate.dayOfMonth) { // same month
          Date(calculationDate.year, calculationDate.month, d)
        }

        else {
          val prevMonth = calculationDate.addMonths(-1)
          Date(prevMonth.year, prevMonth.month, d, true)
        }

      case None => calculationDate
  	}

    val couponEventDate = {
      val d = baseDate.advance(fixingCalendar, -couponNotice, TimeUnit.Days)
      fixingAdjustmentCalendar match {
        case Some(cals) => d.adjust(cals, fixingAdjustmentConvention)
        case _ => d
      }
    }

    val callEventDate = {
      val d = callNotice match {
        case None => couponEventDate
        case Some(d) =>
          val callBaseDate = {
            if (fixingOnCalculationEndDate) endDate
            else paymentDate
          }
          callBaseDate.advance(fixingCalendar, -d, TimeUnit.Days)
      }
  
      fixingAdjustmentCalendar match {
        case Some(cals) => d.adjust(cals, fixingAdjustmentConvention)
        case _ => d
      }
    }

    new CalculationPeriod(
      eventDate = couponEventDate,
      callEventDate = callEventDate,
      startDate = startDate,
      endDate = endDate,
      paymentDate = paymentDate,
      daycounter = daycounter,
      isRedemption = isRedemption,
      nominal = nominal
    )
  }
  
  def simpleCashflow(
    paymentDate:Date,
    paymentCalendar:Calendar,
    paymentConvention:BusinessDayConvention,
    isRedemption: Boolean
  ):CalculationPeriod = {

    apply(
      startDate = paymentDate,
      endDate = paymentDate,
      couponNotice = 0,
      callNotice = None,
      inArrears = false,
      daycounter = new Absolute,
      fixingCalendar = paymentCalendar,
      fixingAdjustmentCalendar = None,
      fixingAdjustmentConvention = BusinessDayConvention.Following,
      paymentCalendar = paymentCalendar,
      paymentConvention = paymentConvention,
      isRedemption = isRedemption,
      nominal = 1.0,
      fixedDayOfMonth = None,
      fixingOnCalculationEndDate = false
    )

  }
  
}
