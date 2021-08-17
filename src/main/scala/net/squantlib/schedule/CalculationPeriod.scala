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
  callValueDate:Date,
  var daycounter:DayCounter,
  isFinalCoupon:Boolean,
  isRedemption: Boolean,
  var nominal:Double
) {

  private var adjustedEventDate:Option[Date] = None

  def getEventDate:Date = getAdjustedEventDate.getOrElse(eventDate)

  def getAdjustedEventDate:Option[Date] = adjustedEventDate

  def setAdjustedEventDate(d:Date) = {
    adjustedEventDate = Some(d)
  }

  def clearAdjustedEventDate = {
    adjustedEventDate = None
  }

  private var adjustedStartDate:Option[Date] = None

  def getStartDate:Date = getAdjustedStartDate.getOrElse(startDate)

  def getAdjustedStartDate:Option[Date] = adjustedStartDate

  def setAdjustedStartDate(d:Date) = {
    adjustedStartDate = Some(d)
  }

  def clearAdjustedStartDate = {
    adjustedStartDate = None
  }

  private var adjustedEndDate:Option[Date] = None

  def getEndDate:Date = getAdjustedEndDate.getOrElse(endDate)

  def getAdjustedEndDate:Option[Date] = adjustedEndDate

  def setAdjustedEndDate(d:Date) = {
    adjustedEndDate = Some(d)
  }

  def clearAdjustedEndDate = {
    adjustedEndDate = None
  }

  private var adjustedPaymentDate:Option[Date] = None

  def getPaymentDate:Date = getAdjustedPaymentDate.getOrElse(paymentDate)

  def getCallValueDate:Date = getAdjustedPaymentDate.getOrElse(callValueDate)

  def irregularCallValueDate:Boolean = callValueDate != paymentDate

  def getAdjustedPaymentDate:Option[Date] = adjustedPaymentDate

  def setAdjustedPaymentDate(d:Date) = {
    adjustedPaymentDate = Some(d)
  }

  def clearAdjustedPaymentDate = {
    adjustedPaymentDate = None
  }

  def clearAdjustedSchedule:Unit = {
    clearAdjustedEventDate
    clearAdjustedStartDate
    clearAdjustedEndDate
    clearAdjustedPaymentDate
  }


  def dayCount:Double = Date.daycount(getStartDate, getEndDate, daycounter)

  def dayCountToCallValueDate:Double = Date.daycount(getStartDate, getAdjustedEndDate.getOrElse(callValueDate), daycounter)

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
      callValueDate = callValueDate.add(shift),
      daycounter = daycounter,
      isFinalCoupon = isFinalCoupon,
      isRedemption = isRedemption,
      nominal = nominal
    )
  }

  override def toString = {
    getEventDate.toString + " " + getStartDate.toString + " " + getEndDate.toString + " " + getPaymentDate.toString + " " + daycounter.toString
  }

  def getRedemptionLeg(nom:Double):CalculationPeriod = {
    CalculationPeriod(
      eventDate = getEventDate,
      callEventDate = callEventDate,
      startDate = if (irregularCallValueDate) getCallValueDate else getEndDate,
      endDate = if (irregularCallValueDate) getCallValueDate else getEndDate,
      paymentDate = if (irregularCallValueDate) getCallValueDate else getPaymentDate,
      callValueDate = callValueDate,
      daycounter = new Absolute,
      isFinalCoupon = false,
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
    couponFixingDate:Option[Date],
    callFixingDate:Option[Date],
    callValueDate:Option[Date],
    daycounter:DayCounter,
    fixingCalendar:Calendar,
    fixingAdjustmentCalendar:Option[Calendar],
    fixingAdjustmentConvention:BusinessDayConvention,
    paymentCalendar:Calendar,
    paymentConvention:BusinessDayConvention,
    isFinalCoupon: Boolean,
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

    val couponEventDate:Date = couponFixingDate.getOrElse({
      val d = baseDate.advance(fixingCalendar, -couponNotice, TimeUnit.Days)
      fixingAdjustmentCalendar match {
        case Some(cals) => d.adjust(cals, fixingAdjustmentConvention)
        case _ => d
      }
    })

    val callEventDate:Date = callFixingDate.getOrElse({
      val callDate = callNotice match {
        case None => couponEventDate
        case Some(notice) if inArrears || fixedDayOfMonth.isDefined || fixingOnCalculationEndDate =>
          callValueDate.getOrElse(baseDate).advance(fixingCalendar, -notice, TimeUnit.Days)
        case Some(notice) =>
          callValueDate.getOrElse(paymentDate).advance(fixingCalendar, -notice, TimeUnit.Days)
      }
  
      fixingAdjustmentCalendar match {
        case Some(cals) => callDate.adjust(cals, fixingAdjustmentConvention)
        case _ => callDate
      }
    })

    new CalculationPeriod(
      eventDate = couponEventDate,
      callEventDate = callEventDate,
      startDate = startDate,
      endDate = endDate,
      paymentDate = paymentDate,
      callValueDate = callValueDate.getOrElse(paymentDate),
      daycounter = daycounter,
      isFinalCoupon = isFinalCoupon,
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
      couponFixingDate = None,
      callFixingDate = None,
      callValueDate = None,
      daycounter = new Absolute,
      fixingCalendar = paymentCalendar,
      fixingAdjustmentCalendar = None,
      fixingAdjustmentConvention = BusinessDayConvention.Following,
      paymentCalendar = paymentCalendar,
      paymentConvention = paymentConvention,
      isFinalCoupon = false,
      isRedemption = isRedemption,
      nominal = 1.0,
      fixedDayOfMonth = None,
      fixingOnCalculationEndDate = false
    )

  }
  
}
