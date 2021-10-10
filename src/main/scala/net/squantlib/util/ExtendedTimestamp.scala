package net.squantlib.util

import java.util.{Date => JavaDate, Calendar => JavaCalendar}
import java.text.SimpleDateFormat
import java.sql.Timestamp
import net.squantlib.util.ql.time.{Date => qlDate, Period => qlPeriod}
import net.squantlib.util.ql.time.Weekday
import net.squantlib.util.ql.daycounters.DayCounter
import net.squantlib.util.ql.time.Calendar
import net.squantlib.util.ql.time.TimeUnit
import net.squantlib.util.ql.time.BusinessDayConvention

object TimestampUtils {
  
  implicit def timestampToExtendedTimestamp(t:Timestamp) = ExtendedTimestamp(t)

  case class ExtendedTimestamp(t:Timestamp) {
    
    def add(y:Int, m:Int, d:Int, h:Int, mm:Int, s:Int):Timestamp = {
      import java.util.Calendar
      val cal = Calendar.getInstance
      cal.setTime(t)
      cal.add(Calendar.YEAR, y)
      cal.add(Calendar.MONTH, m)
      cal.add(Calendar.DAY_OF_WEEK, d)
      cal.add(Calendar.HOUR, h)
      cal.add(Calendar.MINUTE, mm)
      cal.add(Calendar.SECOND, s)
      new Timestamp(cal.getTimeInMillis)
    }

    def addHours(n:Int) = add(0, 0, 0, n, 0, 0)
    def addMinutes(n:Int) = add(0, 0, 0, 0, n, 0)
    def addSeconds(n:Int) = add(0, 0, 0, 0, 0, n)

  }

}


