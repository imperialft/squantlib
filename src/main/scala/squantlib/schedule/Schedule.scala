package squantlib.schedule

import scala.collection.mutable.MutableList
import scala.collection.LinearSeq
import org.jquantlib.daycounters._
import org.jquantlib.time.{Date => qlDate, _}
import org.jquantlib.time.calendars.NullCalendar
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.DateGeneration.Rule._
import java.util.{Date => JavaDate}
import squantlib.model.rates.DiscountCurve

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
    
    val effectiveDate:Option[qlDate] = if (isEmpty) None else Some(dates.minBy(_.startDate).startDate)
    val terminationDate:Option[qlDate] = if (isEmpty) None else Some(dates.maxBy(_.endDate).endDate)
    
    var defaultDaycounter = new Actual365Fixed
    
    def startDate(i:Int):qlDate = dates(i).startDate
    val startDates:List[qlDate] = dates.map(_.startDate)
    def startYears(baseDate:qlDate):List[Double] = dates.map(d => defaultDaycounter.yearFraction(baseDate, d.startDate))
    
    def endDate(i:Int):qlDate = dates(i).endDate
    val endDates:List[qlDate] = dates.map(_.endDate)
    def endYears(baseDate:qlDate):List[Double] = dates.map(d => defaultDaycounter.yearFraction(baseDate, d.endDate))
    
    def eventDate(i:Int):qlDate = dates(i).eventDate
    val eventDates:List[qlDate] = dates.map(_.eventDate)
    def eventYears(baseDate:qlDate):List[Double] = dates.map(d => defaultDaycounter.yearFraction(baseDate, d.eventDate))
    
    def paymentDate(i:Int):qlDate = dates(i).paymentDate
    val paymentDates:List[qlDate] = dates.map(_.paymentDate)
    def paymentYears(baseDate:qlDate):List[Double] = dates.map(d => defaultDaycounter.yearFraction(baseDate, d.paymentDate))

    def currentPeriods(ref:qlDate):List[CalculationPeriod] = dates.filter(d => (ref ge d.startDate) && (ref lt d.endDate))
    
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
    effectiveDate:qlDate,
	terminationDate:qlDate,
	tenor:Period,
	calendar:Calendar,
	calendarConvention:BusinessDayConvention,
	paymentConvention:BusinessDayConvention,
	terminationDateConvention:BusinessDayConvention,
	rule:DateGeneration.Rule,
	fixingInArrears:Boolean,
	noticeDay:Int,
	daycount:DayCounter,
	firstDate:Option[qlDate],
	nextToLastDate:Option[qlDate],
	addRedemption:Boolean,
	maturityNotice:Int):Schedule = {
  
	assert(firstDate.isEmpty || firstDate.get.gt(effectiveDate))
	assert(nextToLastDate.isEmpty || nextToLastDate.get.lt(terminationDate))
	
    val nullCalendar = new NullCalendar
    
    def calcperiod(startdate:qlDate, enddate:qlDate):CalculationPeriod = 
      CalculationPeriod(startdate, enddate, noticeDay, fixingInArrears, daycount, calendar, if (enddate == terminationDate) terminationDateConvention else paymentConvention)
    
    val redemptionLegs:List[CalculationPeriod] = 
      if(addRedemption) List(CalculationPeriod(effectiveDate, terminationDate, maturityNotice, true, new Absolute, calendar, terminationDateConvention))
      else List.empty
    
    if (tenor.length == 0) {return null}
      
    val couponLegs:List[CalculationPeriod] = rule match {
	  
	  case Zero => List(calcperiod(effectiveDate, terminationDate))

      case Backward => 
	  	var tempdates:MutableList[CalculationPeriod] = MutableList.empty
        
        val initialDate = nextToLastDate match {
          case Some(d) => tempdates += calcperiod(d, terminationDate); d
          case None => terminationDate
        }
        
        var periods=1
        var startDate:qlDate = initialDate
        var endDate:qlDate = terminationDate

        do {
          endDate = startDate
          startDate = nullCalendar.advance(initialDate, tenor.mul(periods).negative, calendarConvention)
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
        var startDate:qlDate = effectiveDate
        var endDate:qlDate = initialDate

        do {
          startDate = endDate
          endDate = nullCalendar.advance(initialDate, tenor.mul(periods), calendarConvention)
          if (Math.abs(terminationDate.sub(endDate)) < 14) {endDate = terminationDate}
          tempdates += calcperiod(startDate, if (endDate ge terminationDate) terminationDate else endDate)
          periods = periods + 1
        } while (endDate lt terminationDate)
          
        tempdates.sortBy(_.eventDate).toList

      case _ => 
        println("Unknown schedule rule")
        List.empty
    }
    
    new Schedule(couponLegs ++ redemptionLegs)
  }

  def apply(
    effectiveDate:JavaDate,
	terminationDate:JavaDate,
	tenor:Period,
	calendar:Calendar,
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
	    new qlDate(effectiveDate), 
	    new qlDate(terminationDate), 
	    tenor, 
	    calendar, 
	    calendarConvention, 
	    paymentConvention,
	    terminationDateConvention, 
	    rule, 
	    fixingInArrears, 
	    noticeDay, 
	    daycount, 
	    firstDate.
	    collect{case d => new qlDate(d)}, 
	    nextToLastDate.collect{case d => new qlDate(d)}, 
	    addRedemption, 
	    maturityNotice)

		    
  def periodicalDates(
    startDate:qlDate, 
    endDate:qlDate, 
    period:Period, 
    convention:BusinessDayConvention, 
    calendar:Calendar):List[qlDate] = {
	  
	var periods = 1
	var currentPeriod = 0
	var currentDate = startDate
	var tempdates:MutableList[qlDate] = MutableList(currentDate)
	do {
      currentDate = calendar.advance(startDate, period.mul(periods), convention)
      tempdates += (if (currentDate ge endDate) endDate else currentDate)
      periods = periods + 1
	} while (endDate gt currentDate)
	tempdates.toList
  }

}

