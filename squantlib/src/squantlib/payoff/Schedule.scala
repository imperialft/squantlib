package squantlib.payoff

import org.jquantlib.time._
import org.jquantlib.time.calendars.NullCalendar
import org.jquantlib.daycounters.DayCounter
import scala.collection.mutable.MutableList
import org.jquantlib.time.DateGeneration.Rule._
import scala.collection.immutable.LinearSeq
import org.jquantlib.daycounters.Absolute
import squantlib.model.rates.DiscountCurve
import org.jquantlib.daycounters.Actual365Fixed

class Schedule(val dates:List[CalcPeriod]) extends LinearSeq[CalcPeriod] {
 
	def sort:Schedule = if (isEmpty) this else Schedule(dates.sortBy(_.eventDate))

	def sortWith[A](obj:LinearSeq[A]):(Schedule, LinearSeq[A]) = (dates zip obj).sortBy(_._1.eventDate).unzip match {
	  case (datelist, objlist) => (Schedule(datelist), objlist)
	}
	
    def apply(i:Int):CalcPeriod = dates(i)
	override def isEmpty:Boolean = dates.isEmpty
	override def head:CalcPeriod = dates.head
	override def tail = dates.tail
	override def length = dates.length
	override def iterator:Iterator[CalcPeriod] = dates.iterator

    def get(i:Int):CalcPeriod = dates(i)
    
    val effectiveDate:Option[Date] = if (isEmpty) None else Some(dates.minBy(_.startDate).startDate)
    val terminationDate:Option[Date] = if (isEmpty) None else Some(dates.maxBy(_.endDate).endDate)
    
    var defaultDaycounter = new Actual365Fixed
    
    def startDate(i:Int):Date = dates(i).startDate
    val startDates:List[Date] = dates.map(_.startDate)
    def startYears(baseDate:Date):List[Double] = dates.map(d => defaultDaycounter.yearFraction(baseDate, d.startDate))
    
    def endDate(i:Int):Date = dates(i).endDate
    val endDates:List[Date] = dates.map(_.endDate)
    def endYears(baseDate:Date):List[Double] = dates.map(d => defaultDaycounter.yearFraction(baseDate, d.endDate))
    
    def eventDate(i:Int):Date = dates(i).eventDate
    val eventDates:List[Date] = dates.map(_.eventDate)
    def eventYears(baseDate:Date):List[Double] = dates.map(d => defaultDaycounter.yearFraction(baseDate, d.eventDate))
    
    def paymentDate(i:Int):Date = dates(i).paymentDate
    val paymentDates:List[Date] = dates.map(_.paymentDate)
    def paymentYears(baseDate:Date):List[Double] = dates.map(d => defaultDaycounter.yearFraction(baseDate, d.paymentDate))

    def currentPeriods(ref:Date):List[CalcPeriod] = dates.filter(d => (ref ge d.startDate) && (ref lt d.endDate))
    
    def dayCount(i:Int):Double = dates(i).dayCount
    val dayCounts:List[Double] = dates.map(_.dayCount)
    
    def zeroCoupon(i:Int, curve:DiscountCurve):Double = dates(i).zeroCoupon(curve)
    def zeroCoupons(curve:DiscountCurve):List[Double] = dates.map(_.zeroCoupon(curve))
    
    def coefficient(i:Int, curve:DiscountCurve):Double = dayCount(i) * zeroCoupon(i, curve)
    def coefficients(curve:DiscountCurve):List[Double] = (dayCounts, zeroCoupons(curve)).zipped.map(_ * _)
    
    override def toList:List[CalcPeriod] = dates
    
    override def toString = "eventdate startdate enddate paymentdate\n" + dates.mkString("\n")
    
}

object Schedule{
	
	def empty:Schedule = new Schedule(List.empty)
  
	def apply(inputDates:List[CalcPeriod]) = new Schedule(inputDates)
	
	def apply(
	    effectiveDate:Date,
		terminationDate:Date,
		tenor:Period,
		calendar:Calendar,
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
		maturityNotice:Int) = {
	  
		assert(firstDate.isEmpty || firstDate.get.gt(effectiveDate))
		assert(nextToLastDate.isEmpty || nextToLastDate.get.lt(terminationDate))
		
	    val nullCalendar = new NullCalendar
	    
	    def calcperiod(startdate:Date, enddate:Date):CalcPeriod = 
	      CalcPeriod(startdate, enddate, noticeDay, fixingInArrears, daycount, calendar, if (enddate == terminationDate) terminationDateConvention else paymentConvention)
	    
	    val redemptionLegs:List[CalcPeriod] = 
	      if(addRedemption) List(CalcPeriod(effectiveDate, terminationDate, maturityNotice, true, new Absolute, calendar, terminationDateConvention))
	      else List.empty
	    
	    val couponLegs:List[CalcPeriod] = (rule match {
		  
		  case Zero => List(calcperiod(effectiveDate, terminationDate))
	
	      case Backward => 
		  	var tempdates:MutableList[CalcPeriod] = MutableList.empty
	        
	        val initialDate = nextToLastDate match {
	          case Some(d) => tempdates += calcperiod(d, terminationDate); d
	          case None => terminationDate
	        }
	        
	        var periods=1
	        var startDate:Date = initialDate
	        var endDate:Date = terminationDate
	
	        do {
	          endDate = startDate
	          startDate = nullCalendar.advance(initialDate, tenor.mul(periods).negative, calendarConvention)
	          if (Math.abs(effectiveDate.sub(startDate)) < 14) {startDate = effectiveDate}
	          tempdates += calcperiod(if (startDate lt effectiveDate) effectiveDate else startDate, endDate)
	          periods = periods + 1
	        } while (startDate gt effectiveDate)
	         
	        tempdates.sortBy(_.eventDate).toList
	
	      case Forward =>
		  	var tempdates:MutableList[CalcPeriod] = MutableList.empty
	        
	        val initialDate = firstDate match {
	          case Some(d) => tempdates += calcperiod(effectiveDate, d); d
	          case None => effectiveDate
	        }
	        
	        var periods=1
	        var startDate:Date = effectiveDate
	        var endDate:Date = initialDate
	
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
	    })
	    
	    new Schedule(couponLegs ++ redemptionLegs)
	}

}


case class CalcPeriod(eventDate:Date, startDate:Date, endDate:Date, paymentDate:Date, daycounter:DayCounter) {
  
	def dayCount:Double = daycounter.yearFraction(startDate, endDate)
	
    def isCurrentPeriod(ref:Date):Boolean = (ref ge startDate) && (ref lt endDate)
    
    def accrued(ref:Date):Double = if (isCurrentPeriod(ref)) daycounter.yearFraction(startDate, ref) else 0.0
    
    def dayCountAfter(ref:Date):Double = 
      if (startDate ge ref) dayCount
      else if (isCurrentPeriod(ref)) daycounter.yearFraction(ref, endDate) 
      else 0.0
    
    def zeroCoupon(curve:DiscountCurve):Double = curve(paymentDate)
    
    def coefficient(curve:DiscountCurve):Double = dayCount * zeroCoupon(curve)
    
    def isAbsolute:Boolean = daycounter match {
        case d:Absolute => true
        case _ => false
      }
  
	override def toString = eventDate.shortDate.toString + " " + startDate.shortDate.toString + " " + endDate.shortDate.toString + " " + paymentDate.shortDate.toString + " " + daycounter.toString
}

object CalcPeriod {
  
	def apply(startDate:Date, endDate:Date, notice:Int, inarrears:Boolean, daycount:DayCounter, calendar:Calendar, paymentConvention:BusinessDayConvention):CalcPeriod = {
	  val eventDate = if (inarrears) calendar.advance(endDate, -notice, TimeUnit.Days)
			  		else calendar.advance(startDate, -notice, TimeUnit.Days)
			  		  
	  val paymentDate = calendar.adjust(endDate, paymentConvention)
	  				
	  new CalcPeriod(eventDate, startDate, endDate, paymentDate, daycount)
	}
  
}
