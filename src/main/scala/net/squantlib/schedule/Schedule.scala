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

class Schedule(val dates:List[CalculationPeriod]) extends LinearSeq[CalculationPeriod] {
 
  def sort:Schedule = if (isEmpty) this else Schedule(dates.sortBy(_.eventDate))

  def sortWith[A](obj:LinearSeq[A]):LinearSeq[(CalculationPeriod, A)] = (dates zip obj).sortBy{case (d, _) => (d.paymentDate, d.dayCount)}
  
  def sortWith[A, B](obj1:LinearSeq[A], obj2:LinearSeq[B]):LinearSeq[(CalculationPeriod, A, B)] =
    (dates, obj1, obj2).zipped.toList.sortBy{case (d, _, _) => (d.paymentDate, d.dayCount)}
  
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
    fixingCalendar: Calendar,
    paymentCalendar:Calendar,
    calendarConvention:BusinessDayConvention,
    paymentConvention:BusinessDayConvention,
    terminationDateConvention:BusinessDayConvention,
    rule:DateGeneration.Rule,
    fixingInArrears:Boolean,
    noticeDay:Int,
    daycount:DayCounter,
    firstDate:Option[Date],
    nextToLastDate:Option[Date],
    addRedemption:Boolean,
    maturityNotice:Int):Schedule = {
    
    assert(firstDate.isEmpty || firstDate.get.gt(effectiveDate))
    assert(nextToLastDate.isEmpty || nextToLastDate.get.lt(terminationDate))
    
    val nullCalendar = Calendars.empty
    
    def calcperiod(startdate:Date, enddate:Date):CalculationPeriod = 
      CalculationPeriod(startdate, enddate, noticeDay, fixingInArrears, daycount, fixingCalendar, paymentCalendar, if (enddate == terminationDate) terminationDateConvention else paymentConvention)
    
    val redemptionLegs:List[CalculationPeriod] = 
      if(addRedemption) List(CalculationPeriod(effectiveDate, terminationDate, maturityNotice, true, new Absolute, fixingCalendar, paymentCalendar, terminationDateConvention))
      else List.empty
    
    val couponLegs:List[CalculationPeriod] = 
      if (tenor.length == 0) List.empty
      else rule match {
        
        case Zero => List(calcperiod(effectiveDate, terminationDate))
  
        case Backward => 
          var tempdates:MutableList[CalculationPeriod] = MutableList.empty
            
          val initialDate = nextToLastDate match {
            case Some(d) => tempdates += calcperiod(d, terminationDate); d
            case None => terminationDate
          }
          
          var periods=1
          var startDate:Date = initialDate
          var endDate:Date = terminationDate
  
          do {
            endDate = startDate
            startDate = initialDate.advance(nullCalendar, tenor.mul(periods).negative, calendarConvention)
            if (Math.abs(effectiveDate.sub(startDate)) < 14) {startDate = effectiveDate}
            tempdates += calcperiod(if (startDate lt effectiveDate) effectiveDate else startDate, endDate)
            periods = periods + 1
          } while (startDate gt effectiveDate)
           
          tempdates.sortBy(_.eventDate).toList
  
        case Forward =>
          var tempdates:MutableList[CalculationPeriod] = MutableList.empty
            
          val initialDate = firstDate match {
            case Some(d) => tempdates += calcperiod(effectiveDate, d); d
            case None => effectiveDate
          }
          
          var periods=1
          var startDate:Date = effectiveDate
          var endDate:Date = initialDate
  
          do {
            startDate = endDate
            endDate = initialDate.advance(nullCalendar, tenor.mul(periods), calendarConvention)
            if (Math.abs(terminationDate.sub(endDate)) < 14) {endDate = terminationDate}
            tempdates += calcperiod(startDate, if (endDate ge terminationDate) terminationDate else endDate)
            periods = periods + 1
          } while (endDate lt terminationDate)
            
          tempdates.sortBy(_.eventDate).toList
  
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
    paymentCalendar:Calendar,
    calendarConvention:BusinessDayConvention,
    paymentConvention:BusinessDayConvention,
    terminationDateConvention:BusinessDayConvention,
    rule:DateGeneration.Rule,
    fixingInArrears:Boolean,
    noticeDay:Int,
    daycount:DayCounter,
    firstDate:Option[JavaDate],
    nextToLastDate:Option[JavaDate],
    addRedemption:Boolean,
    maturityNotice:Int):Schedule 
    = apply(
      Date(effectiveDate), 
      Date(terminationDate), 
      tenor, 
      fixingCalendar,
      paymentCalendar, 
      calendarConvention, 
      paymentConvention,
      terminationDateConvention, 
      rule, 
      fixingInArrears, 
      noticeDay, 
      daycount, 
      firstDate.collect{case d => Date(d)}, 
      nextToLastDate.collect{case d => Date(d)}, 
      addRedemption, 
      maturityNotice)

        
  def periodicalDates(
    startDate:Date, 
    endDate:Date, 
    period:Period, 
    convention:BusinessDayConvention, 
    paymentCalendar:Calendar
  ):List[Date] = {
    
    var periods = 1
    var currentPeriod = 0
    var currentDate = startDate
    var tempdates:MutableList[Date] = MutableList(currentDate)
    do {
        currentDate = Date(paymentCalendar.advance(startDate.ql, period.mul(periods), convention))
        tempdates += (if (currentDate ge endDate) endDate else currentDate)
        periods = periods + 1
    } while (endDate gt currentDate)
    tempdates.toList
  }

}

