package net.squantlib.util

import java.util.{Date => JavaDate, Calendar => JavaCalendar}
import java.text.SimpleDateFormat
import java.sql.Timestamp
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod}
import org.jquantlib.time.Weekday
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.Calendar
import org.jquantlib.time.TimeUnit
import org.jquantlib.time.BusinessDayConvention
import scala.collection.LinearSeq
import scala.annotation.tailrec

trait Date extends Ordered[Date] with Cloneable with Serializable{
  def java:JavaDate
  
  def ql:qlDate
  
  def ge(d:Date):Boolean = ql ge d.ql
  
  def le(d:Date):Boolean = ql le d.ql 
  
  def gt(d:Date):Boolean = ql gt d.ql
  
  def lt(d:Date):Boolean = ql lt d.ql
  
  def eq(d:Date):Boolean = ql eq d.ql
  
  override def compare(d: Date):Int = ql compareTo d.ql
  
  override def hashCode:Int = ql.serialNumber.toInt

  override def equals(obj:Any):Boolean = obj match {
    case b if b == null => false
    case b:Date if b.ql eq ql => true
    case _ => false
  }

  def isWeekday:Boolean = ql.weekday != Weekday.Saturday && ql.weekday != Weekday.Sunday
  
  def isBusinessday(calendar:Calendar):Boolean = calendar.isBusinessDay(ql)
  
  def add(d:Int):Date = Date(ql add d)
  
  def add(p:qlPeriod):Date = Date(ql add p)
  
  def addWeekday(d:Int):Date = addWeekdayRec(d, 1)
  
  @tailrec private def addWeekdayRec(d:Int, shift:Int):Date = 
    if(d == 0) this 
    else add(shift) match {
      case date if date.isWeekday => date.addWeekdayRec(d - 1, shift)
      case date => date.addWeekdayRec(d, shift)
    }
  
  def subWeekday(d:Int):Date = addWeekdayRec(d, -1)
  
  def advance(calendar:Calendar, d:Int):Date = advance(calendar, d, TimeUnit.Days)
  
  def advance(calendar:Calendar, d:Int, unit:TimeUnit):Date = Date(calendar.advance(ql, d, unit))
  
  def advance(calendar:Calendar, period:qlPeriod, convention:BusinessDayConvention = BusinessDayConvention.Following):Date = Date(calendar.advance(ql, period, convention))
  
  def adjust(calendar:Calendar, convention:BusinessDayConvention):Date = Date(calendar.adjust(ql, convention))
  
  def sub(d:Date):Long = ql sub d.ql
  
  def sub(d:Int):Date= Date(ql sub d)

  def beginningOfMonth:Date = Date(year, month, 1)
  
  //def endOfMonth:Date = addMonths(1).beginningOfMonth.sub(1)
  def endOfMonth:Date = Date(year, month, Date.lastDayOfMonth(year, month))
  
  def serialNumber:Long = ql.serialNumber
  
  def days(p:qlPeriod):Long = p.days(ql)
  
  def months(p:qlPeriod):Double = qlPeriod.months(p, ql)
  
  def min(d:Date):Date = Date.min(this, d)
  
  def max(d:Date):Date = Date.max(this, d)
  
  def dayOfMonth:Int = ql.dayOfMonth
  
  def month:Int = ql.month.value
  
  def addMonths(m:Int):Date = Date(this.ql.add(new qlPeriod(m, TimeUnit.Months)))
  
  def year:Int = ql.year
  
  def copy = Date(serialNumber)
  
  def yyyymmdd(yString:String = "", mString:String = "", dString:String = ""):String = s"""%tY${yString}%<tm${mString}%<td${dString}""".format(java)
  
  def getTimestamp:Timestamp = new Timestamp(java.getTime)
  
  def getTimestamp(hour:Int, minute:Int, second:Int, millisecond:Int = 0):Timestamp = {
    val cal = JavaCalendar.getInstance()
    cal.setTime(java)
    cal.set(JavaCalendar.HOUR_OF_DAY, hour)
    cal.set(JavaCalendar.MINUTE, minute)
    cal.set(JavaCalendar.SECOND, second)
    cal.set(JavaCalendar.MILLISECOND, millisecond)
    new Timestamp(cal.getTimeInMillis)
  }
  
  override def toString = yyyymmdd("/", "/", "")

  def toSqlString = yyyymmdd("-", "-", "")
}

object Date {
  
  def apply(d:JavaDate):Date = new JavaDateImpl(d)
  
  def apply(d:qlDate):Date = new QlDateImpl(d)
  
  def apply(d:Long):Date = new QlDateImpl(new qlDate(d))
  
  def apply(day:Int, month:Int, year:Int):Date = {
    // (year, month, day)
    if (day >= 1900) new QlDateImpl(new qlDate(year, month, day))
    // (day, month, year)
    else new QlDateImpl(new qlDate(day, month, year))
  }
  
  def apply(day:Int, month:Int, year:Int, adjustMonthEnd:Boolean):Date = {
    if (adjustMonthEnd == false) apply(day, month, year)
    else if (day >= 1900) apply(day, month, Math.min(lastDayOfMonth(day, month), year))
    else apply(Math.min(lastDayOfMonth(year, month), day), month, year)
  }
  
//  def apply(t:Timestamp):Date = apply(t.getTime)
  
  def getDate(d:String, stringFormat:String = null):Option[Date] = {
    if (d == null || d == "" )
      None

    val sFormat:String = {
      if (stringFormat != null) stringFormat
      else if (d.contains("/")) "yyyy/MM/dd"
      else "yyyy-MM-dd"
    }
    val dateFormat = new SimpleDateFormat(sFormat)
    dateFormat.setLenient(false)
    try {
      Some(Date(dateFormat.parse(d)))
    } catch {case e:Exception  =>
      None
    } 
  }
  
  def currentDate:Date = apply(currentTime)
  
  def currentTime:JavaDate = JavaCalendar.getInstance.getTime

  def currentTimeString(datetimeFormat:String = "%tY/%<tm/%<td %<tH:%<tM:%<tS") = datetimeFormat.format(currentTime)
  
  def currentTimestamp:Timestamp = new Timestamp(currentTime.getTime)
  
  def daycount(d1:Date, d2:Date, daycounter:DayCounter):Double = daycounter.yearFraction(d1.ql, d2.ql)
  
  def daysBetween(fromDate:Date, toDate:Date):Set[Date] = (fromDate.serialNumber to toDate.serialNumber).map(apply)(collection.breakOut)
  
  def weekdaysBetween(fromDate:Date, toDate:Date):Set[Date] = daysBetween(fromDate, toDate).filter(_.isWeekday)
  
  def businessDaysBetween(fromDate:Date, toDate:Date, calendar:Calendar) = daysBetween(fromDate, toDate).filter(_.isBusinessday(calendar))
  
  def holidaysBetween(fromDate:Date, toDate:Date, calendar:Calendar) = daysBetween(fromDate, toDate).filter(d => !d.isBusinessday(calendar))

  def min(d1:Date, d2:Date):Date = Date(if (d1 lt d2) d1.serialNumber else d2.serialNumber)
  
  def max(d1:Date, d2:Date):Date = Date(if (d1 gt d2) d1.serialNumber else d2.serialNumber)

  val monthEnds:Map[Int, Map[Int, Int]] = (1990 to 2100).map(yr => (yr, (1 to 12).map(mth => (mth, Date(yr, mth, 1).addMonths(1).sub(1).dayOfMonth)).toMap)).toMap
  
  def lastDayOfMonth(yr:Int, m:Int):Int = {
    if (yr >= monthEnds.keys.min && yr <= monthEnds.keys.max) monthEnds(yr)(m)
    else Date(yr, m, 1).addMonths(1).sub(1).dayOfMonth
  }
  
}

class JavaDateImpl(d:JavaDate) extends Date{
  lazy val qldate = new qlDate(d)
  override def java:JavaDate = d
  override def ql:qlDate = qldate
}

class QlDateImpl(d:qlDate) extends Date{
  lazy val javadate = d.longDate
  override def java = javadate
  override def ql = d
}