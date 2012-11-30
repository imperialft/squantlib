package squantlib.payoff

import org.jquantlib.time._
import org.jquantlib.time.calendars.NullCalendar
import scala.collection.mutable.MutableList
import org.jquantlib.time.DateGeneration.Rule._

class Schedule(
    val effectiveDate:Date,
	val terminationDate:Date,
	val tenor:Period,
	val calendar:Calendar,
	val calendarConvention:BusinessDayConvention,
	val paymentConvention:BusinessDayConvention,
	val terminationDateConvention:BusinessDayConvention,
	val rule:DateGeneration.Rule,
	val fixingInArrears:Boolean,
	val noticeDay:Int,
	val firstDate:Option[Date],
	val nextToLastDate:Option[Date]) {
  
	assert(firstDate.isEmpty || firstDate.get.gt(effectiveDate))
	assert(nextToLastDate.isEmpty || nextToLastDate.get.lt(terminationDate))
	
    val nullCalendar = new NullCalendar
    
    val dates:List[CalcPeriod] = rule match {
	  
	  case Zero => List(CalcPeriod(effectiveDate, terminationDate))

      case Backward => 
	  	var tempdates:MutableList[CalcPeriod] = MutableList.empty
        
        val initialDate = nextToLastDate match {
          case Some(d) => tempdates += CalcPeriod(d, terminationDate); d
          case None => terminationDate
        }
        
        var periods=1
        var startDate:Date = initialDate
        var endDate:Date = terminationDate

        do {
          endDate = startDate
          startDate = nullCalendar.advance(initialDate, tenor.mul(periods).negative, calendarConvention)
          if (Math.abs(effectiveDate.sub(startDate)) < 14) {startDate = effectiveDate}
          tempdates += CalcPeriod(if (startDate lt effectiveDate) effectiveDate else startDate, endDate)
          periods = periods + 1
        } while (startDate gt effectiveDate)
         
        tempdates.sortBy(_.eventDate).toList

      case Forward =>
	  	var tempdates:MutableList[CalcPeriod] = MutableList.empty
        
        val initialDate = firstDate match {
          case Some(d) => tempdates += CalcPeriod(effectiveDate, d); d
          case None => effectiveDate
        }
        
        var periods=1
        var startDate:Date = effectiveDate
        var endDate:Date = initialDate

        do {
          startDate = endDate
          endDate = nullCalendar.advance(initialDate, tenor.mul(periods), calendarConvention)
          if (Math.abs(terminationDate.sub(endDate)) < 14) {endDate = terminationDate}
          tempdates += CalcPeriod(startDate, if (endDate ge terminationDate) terminationDate else endDate)
          periods = periods + 1
        } while (endDate lt terminationDate)
          
        tempdates.sortBy(_.eventDate).toList

      case _ => 
        println("Unknown schedule rule")
        List.empty
    }

    def size:Int = dates.size

    def get(i:Int):CalcPeriod = dates(i)
    
    def startDate(i:Int):Date = dates(i).startDate
    def startDates:List[Date] = dates.map(_.startDate)
    
    def endDate(i:Int):Date = dates(i).endDate
    def endDates:List[Date] = dates.map(_.endDate)
    
    def eventDate(i:Int):Date = dates(i).eventDate
    def eventDates:List[Date] = dates.map(_.eventDate)
    
    def paymentDate(i:Int):Date = dates(i).paymentDate
    def paymentDates:List[Date] = dates.map(_.paymentDate)

    def currentPeriods(ref:Date) = dates.filter(d => (ref ge d.startDate) && (ref lt d.endDate))
    
    def isEmpty = dates.isEmpty
    
	def CalcPeriod(eventDate:Date, startDate:Date, endDate:Date, paymentDate:Date):CalcPeriod = 
	  new CalcPeriod(eventDate, startDate, endDate, paymentDate)
	
	def CalcPeriod(startDate:Date, endDate:Date):CalcPeriod = {
	  val eventDate = if (fixingInArrears) calendar.advance(endDate, -noticeDay, TimeUnit.Days)
			  		else calendar.advance(startDate, -noticeDay, TimeUnit.Days)
			  		  
	  val paymentDate = if (endDate == terminationDate) calendar.adjust(endDate, terminationDateConvention)
	  				else calendar.adjust(endDate, paymentConvention)
	  
	  new CalcPeriod(eventDate, startDate, endDate, paymentDate)
	}
	
    override def toString = dates.mkString("\n")
    
}

case class CalcPeriod(val eventDate:Date, val startDate:Date, val endDate:Date,val paymentDate:Date) {
	override def toString = "eventdate startdate enddate paymentdate\n" + eventDate.shortDate + " " + startDate.shortDate + " " + endDate.shortDate + " " + paymentDate.shortDate
}
