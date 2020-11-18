package net.squantlib.schedule

import scala.collection.mutable.MutableList
import scala.collection.LinearSeq
import net.squantlib.util.Date
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.initializer.Calendars
import net.squantlib.model.rates.DiscountCurve
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.DateGeneration.Rule._
import org.jquantlib.time.{Period, Calendar, BusinessDayConvention, DateGeneration}
import org.jquantlib.daycounters._
import java.util.{Date => JavaDate}
import Ordering.Tuple2

class Schedule(
  val dates:List[CalculationPeriod]
) extends LinearSeq[CalculationPeriod] {
 
  def sort:Schedule = {
    if (isEmpty) this
    else Schedule(dates.sortBy(_.eventDate))
  }

  def sortWith[A](obj:LinearSeq[A]):LinearSeq[(CalculationPeriod, A)] = (dates zip obj).sortBy{case (d, _) => (d.paymentDate, d.dayCount)}
  
  def sortWith[A, B](obj1:LinearSeq[A], obj2:LinearSeq[B]):LinearSeq[(CalculationPeriod, A, B)] =
    (dates, obj1, obj2).zipped.toList.sortBy{case (d, _, _) => (d.paymentDate, d.dayCount)}

  def sortWith[A, B](obj1:LinearSeq[A], obj2:LinearSeq[B], keepFinalLeg:Boolean):LinearSeq[(CalculationPeriod, A, B)] =
    if (keepFinalLeg) {
      val polist = (dates, obj1, obj2).zipped.toList
      if (polist.size <= 1) polist 
      else (polist.dropRight(1).sortBy{case (d, _, _) => (d.paymentDate, d.dayCount)} :+ polist.last)
    } else sortWith(obj1, obj2)
  
  def apply(i:Int):CalculationPeriod = dates(i)
  
  override def isEmpty:Boolean = dates.isEmpty
  
  override def head:CalculationPeriod = dates.head
  
  override def tail = dates.tail
  
  override def length = dates.length
  
  override def iterator:Iterator[CalculationPeriod] = dates.iterator

  def get(i:Int):CalculationPeriod = dates(i)
  
  val effectiveDate:Option[Date] = if (isEmpty) None else Some(dates.minBy(_.startDate).startDate)
  
  val terminationDate:Option[Date] = if (isEmpty) None else Some(dates.maxBy(_.endDate).endDate)

  var defaultDaycounter = new Actual365Fixed
  
  def startDate(i:Int):Date = dates(i).startDate
  
  val startDates:List[Date] = dates.map(_.startDate)
  
  def startYears(baseDate:Date):List[Double] = dates.map(d => Date.daycount(baseDate, d.startDate, defaultDaycounter))
  
  def endDate(i:Int):Date = dates(i).endDate
  
  val endDates:List[Date] = dates.map(_.endDate)
  
  def endYears(baseDate:Date):List[Double] = dates.map(d => Date.daycount(baseDate, d.endDate, defaultDaycounter))
  
  def eventDate(i:Int):Date = dates(i).eventDate
  
  val eventDates:List[Date] = dates.map(_.eventDate)
  
  def eventYears(baseDate:Date):List[Double] = dates.map(d => Date.daycount(baseDate, d.eventDate, defaultDaycounter))
  
  def paymentDate(i:Int):Date = dates(i).paymentDate
  
  val paymentDates:List[Date] = dates.map(_.paymentDate)
  
  def paymentYears(baseDate:Date):List[Double] = dates.map(d => Date.daycount(baseDate, d.paymentDate, defaultDaycounter))

  def callEventDate(i:Int):Date = dates(i).callEventDate

  val callEventDates:List[Date] = dates.map(_.callEventDate)

  def currentPeriods(ref:Date):List[CalculationPeriod] = dates.filter(d => (ref ge d.startDate) && (ref lt d.endDate))
  
  def dayCount(i:Int):Double = dates(i).dayCount
  
  val dayCounts:List[Double] = dates.map(_.dayCount)
  
  def zeroCoupon(i:Int, curve:DiscountCurve):Double = dates(i).zeroCoupon(curve)
  
  def zeroCoupons(curve:DiscountCurve):List[Double] = dates.map(_.zeroCoupon(curve))
  
  def coefficient(i:Int, curve:DiscountCurve):Double = dayCount(i) * zeroCoupon(i, curve)
  
  def coefficients(curve:DiscountCurve):List[Double] = (dayCounts, zeroCoupons(curve)).zipped.map(_ * _)
  
  def shifted(shift:Int):Schedule = new Schedule(dates.map(d => d.shifted(shift)))
  
  override def toList:List[CalculationPeriod] = dates
  
  override def toString = "eventdate startdate enddate paymentdate\n" + dates.mkString("\n")
    
}

object Schedule{
  
  def empty:Schedule = new Schedule(List.empty)
  
  def apply(inputDates:List[CalculationPeriod]):Schedule = new Schedule(inputDates)
  
  def apply(dates:LinearSeq[CalculationPeriod]):Schedule = new Schedule(dates.toList)
  
  def apply(
    effectiveDate:Date,
    terminationDate:Date,
    tenor:Period,
    fixingCalendar:Calendar,
    fixingAdjustmentCalendar:Option[Calendar],
    fixingAdjustmentConvention:BusinessDayConvention,
    paymentCalendar:Calendar,
    calendarConvention:BusinessDayConvention,
    paymentConvention:BusinessDayConvention,
    terminationDateConvention:BusinessDayConvention,
    rule:DateGeneration.Rule,
    fixingInArrears:Boolean,
    couponNotice:Int,
    redemptionNotice:Option[Int],
    callNotice:Option[Int],
    daycounter:DayCounter,
    firstDate:Option[Date],
    nextToLastDate:Option[Date],
    addRedemption:Boolean,
    fixedDayOfMonth:Option[Int],
    fixingOnCalculationEndDate:Boolean,
    rollMonthEnd: Boolean
  ):Schedule = {
    
    assert(firstDate.isEmpty || firstDate.get.gt(effectiveDate))
    assert(nextToLastDate.isEmpty || nextToLastDate.get.lt(terminationDate))
    
    val nullCalendar = Calendars.empty
    
    def getCalculationPeriod(
      startDate:Date,
      endDate:Date,
      isRedemption: Boolean
    ):CalculationPeriod = {
      CalculationPeriod(
        startDate = startDate,
        endDate = endDate,
        couponNotice = couponNotice,
        callNotice = callNotice,
        inArrears = fixingInArrears,
        daycounter = daycounter,
        fixingCalendar = fixingCalendar,
        fixingAdjustmentCalendar = fixingAdjustmentCalendar,
        fixingAdjustmentConvention = fixingAdjustmentConvention,
        paymentCalendar = paymentCalendar,
        paymentConvention = if (endDate == terminationDate) terminationDateConvention else paymentConvention,
        isRedemption = isRedemption,
        nominal = 1.0,
        fixedDayOfMonth = fixedDayOfMonth,
        fixingOnCalculationEndDate = fixingOnCalculationEndDate
      )
    }

    val redemptionLegs:List[CalculationPeriod] = {
      if(addRedemption) List(CalculationPeriod(
        startDate = effectiveDate,
        endDate = terminationDate,
        couponNotice = redemptionNotice.getOrElse(couponNotice),
        callNotice = callNotice,
        inArrears = true,
        daycounter = new Absolute,
        fixingCalendar = fixingCalendar,
        fixingAdjustmentCalendar = fixingAdjustmentCalendar,
        fixingAdjustmentConvention = fixingAdjustmentConvention,
        paymentCalendar = paymentCalendar,
        paymentConvention = terminationDateConvention,
        isRedemption = true,
        nominal = 1.0,
        fixedDayOfMonth = redemptionNotice match {
          case Some(d) => None
          case _ => fixedDayOfMonth
        },
        fixingOnCalculationEndDate = fixingOnCalculationEndDate
      ))
      else List.empty
    }

    val couponLegs:List[CalculationPeriod] = 
      if (tenor.length == 0) List.empty
      else rule match {
        
        case Zero => List(getCalculationPeriod(effectiveDate, terminationDate, false))
  
        case Backward => 
          var tempDates:MutableList[CalculationPeriod] = MutableList.empty
            
          val initialDate = nextToLastDate match {
            case Some(d) if d lt terminationDate => 
              tempDates += getCalculationPeriod(d, terminationDate, false)
              d
            case None => terminationDate
          }

          val firstEndDateAfter = firstDate.getOrElse(effectiveDate)
          val firstStartDateAfter = effectiveDate.add(
            if (firstDate.isDefined) 0
            else if (effectiveDate.add(tenor).sub(effectiveDate) > 32) 32
            else 14
          )

          var periods = 1
          var startDate:Date = initialDate
          var endDate:Date = terminationDate

          do {
            endDate = startDate
            startDate = initialDate.advance(nullCalendar, tenor.mul(periods).negative, calendarConvention)

            if (rollMonthEnd && (startDate.endOfMonth gt effectiveDate.endOfMonth)) {
              startDate = startDate.endOfMonth
            }

            if (startDate.sub(firstEndDateAfter) <= 0 || startDate.sub(firstStartDateAfter) <= 0) { // if (Math.abs(effectiveDate.sub(startDate)) < 14) {
              startDate = effectiveDate
            }

            tempDates += getCalculationPeriod(if (startDate lt effectiveDate) effectiveDate else startDate, endDate, false)

            periods = periods + 1
          } while (startDate gt effectiveDate)

          tempDates.sortBy(_.eventDate).toList
  
        case Forward =>
          var tempDates:MutableList[CalculationPeriod] = MutableList.empty
            
          val initialDate = firstDate match {
            case Some(d) => tempDates += getCalculationPeriod(effectiveDate, d, false); d
            case None => effectiveDate
          }
          
          var periods=1
          var startDate:Date = effectiveDate
          var endDate:Date = initialDate
  
          do {
            startDate = endDate
            endDate = initialDate.advance(nullCalendar, tenor.mul(periods), calendarConvention)
            if (Math.abs(terminationDate.sub(endDate)) < 14) {endDate = terminationDate}
            tempDates += getCalculationPeriod(startDate, if (endDate ge terminationDate) terminationDate else endDate, false)
            periods = periods + 1
          } while (endDate lt terminationDate)
            
          tempDates.sortBy(_.eventDate).toList
  
        case _ => 
          errorOutput("Unknown schedule rule")
          List.empty
      }
    
    new Schedule(couponLegs ++ redemptionLegs)
  }

  def apply(
    effectiveDate:JavaDate,
    terminationDate:JavaDate,
    tenor:Period,
    fixingCalendar:Calendar,
    fixingAdjustmentCalendar:Option[Calendar],
    fixingAdjustmentConvention:BusinessDayConvention,
    paymentCalendar:Calendar,
    calendarConvention:BusinessDayConvention,
    paymentConvention:BusinessDayConvention,
    terminationDateConvention:BusinessDayConvention,
    rule:DateGeneration.Rule,
    fixingInArrears:Boolean,
    couponNotice:Int,
    redemptionNotice:Option[Int],
    callNotice:Option[Int],
    daycounter:DayCounter,
    firstDate:Option[JavaDate],
    nextToLastDate:Option[JavaDate],
    addRedemption:Boolean,
    fixedDayOfMonth:Option[Int],
    fixingOnCalculationEndDate:Boolean,
    rollMonthEnd: Boolean
  ):Schedule = {
    apply(
      effectiveDate = Date(effectiveDate),
      terminationDate = Date(terminationDate),
      tenor = tenor,
      fixingCalendar = fixingCalendar,
      fixingAdjustmentCalendar = fixingAdjustmentCalendar,
      fixingAdjustmentConvention = fixingAdjustmentConvention,
      paymentCalendar = paymentCalendar,
      calendarConvention = calendarConvention,
      paymentConvention = paymentConvention,
      terminationDateConvention = terminationDateConvention,
      rule = rule,
      fixingInArrears = fixingInArrears,
      couponNotice = couponNotice,
      redemptionNotice = redemptionNotice,
      callNotice = callNotice,
      daycounter = daycounter,
      firstDate = firstDate.collect { case d => Date(d) },
      nextToLastDate = nextToLastDate.collect { case d => Date(d) },
      addRedemption = addRedemption,
      fixedDayOfMonth = fixedDayOfMonth,
      fixingOnCalculationEndDate = fixingOnCalculationEndDate,
      rollMonthEnd = rollMonthEnd
    )
  }

        
  def periodicalDates(
    startDate:Date, 
    endDate:Date, 
    period:Period, 
    convention:BusinessDayConvention, 
    paymentCalendar:Calendar
  ):List[Date] = {
    
    var periods = 1
//    var currentPeriod = 0
    var currentDate = startDate
    var tempDates:MutableList[Date] = MutableList(currentDate)
    do {
        currentDate = Date(paymentCalendar.advance(startDate.ql, period.mul(periods), convention))
        tempDates += (if (currentDate ge endDate) endDate else currentDate)
        periods = periods + 1
    } while (endDate gt currentDate)
    tempDates.toList
  }

}

