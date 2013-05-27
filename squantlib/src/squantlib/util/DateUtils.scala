package squantlib.util

import scala.collection.mutable.MutableList
import org.jquantlib.time.calendars.NullCalendar
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod, BusinessDayConvention, Calendar}

object DateUtils {
  
  def periodicalDates(startDate:qlDate, endDate:qlDate, period:qlPeriod, convention:BusinessDayConvention, calendar:Calendar):List[qlDate] = {
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