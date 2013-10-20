package squantlib.util

import java.util.{Date => JavaDate}
import org.jquantlib.time.{Date => qlDate, Period => qlPeriod}
import org.jquantlib.time.Weekday
import java.sql.Timestamp
import org.jquantlib.daycounters.DayCounter
import org.jquantlib.time.Calendar
import org.jquantlib.time.TimeUnit
import org.jquantlib.time.BusinessDayConvention

@cloneable
trait Date extends Ordered[Date] {
  def java:JavaDate
  
  def ql:qlDate
  
  def ge(d:Date):Boolean = ql ge d.ql
  
  def le(d:Date):Boolean = ql le d.ql
  
  def gt(d:Date):Boolean = ql gt d.ql
  
  def lt(d:Date):Boolean = ql lt d.ql
  
  def compare(d: Date) = ql compareTo d.ql
  
  def isWeekday:Boolean = ql.weekday != Weekday.Saturday && ql.weekday != Weekday.Sunday
  
  def add(d:Int):Date = Date(ql add d)
  
  def add(p:qlPeriod):Date = Date(ql add p)
  
  def advance(calendar:Calendar, d:Int, unit:TimeUnit):Date = Date(calendar.advance(ql, d, unit))
  
  def advance(calendar:Calendar, period:qlPeriod, convention:BusinessDayConvention = BusinessDayConvention.Following):Date = Date(calendar.advance(ql, period, convention))
  
  def adjust(calendar:Calendar, convention:BusinessDayConvention):Date = Date(calendar.adjust(ql, convention))
  
  def sub(d:Date):Long = ql sub d.ql
  
  def serialNumber:Long = ql.serialNumber
  
  def days(p:qlPeriod):Long = p.days(ql)
  
  def months(p:qlPeriod):Double = qlPeriod.months(p, ql)
  
  override def clone() = super.clone().asInstanceOf[Date]
  
  override def toString = "%tY/%<tm/%<td".format(java)
  
}

object Date {
  def apply(d:JavaDate):Date = new JavaDateImpl(d)
  def apply(d:qlDate):Date = new QlDateImpl(d)
  def apply(d:Long):Date = new QlDateImpl(new qlDate(d))
  
  def currentDate:Date = apply(currentTime)
  def currentTime:JavaDate = java.util.Calendar.getInstance.getTime
  def currentTimestamp:Timestamp = new Timestamp(currentTime.getTime)
  
  def daycount(d1:Date, d2:Date, daycounter:DayCounter):Double = daycounter.yearFraction(d1.ql, d2.ql)
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