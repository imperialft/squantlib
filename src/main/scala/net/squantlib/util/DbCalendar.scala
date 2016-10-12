package net.squantlib.util

import org.jquantlib.time.Calendar
import net.squantlib.database.DB
import org.jquantlib.time.Weekday
import org.jquantlib.time.Weekday._
import org.jquantlib.time.{Date => qlDate}

case class DbCalendar(calendar:Calendar, countryIds:Set[String], dbLastDate:qlDate) extends Calendar {
  
  def isDbPeriod(d:qlDate) = d.le(dbLastDate)
  
  lazy val dbHolidays:Set[qlDate] = countryIds.map(id => DB.getHolidays(id)).flatten.map(_.ql)
  
  override def name:String = "DB calendar " + countryIds.mkString(" ")

  override def isWeekend(w:Weekday):Boolean = w == Saturday || w == Sunday

  override def isBusinessDay(date:qlDate):Boolean = 
    if (isDbPeriod(date)) !isWeekend(date.weekday) && !dbHolidays.contains(date)
    else calendar.isBusinessDay(date)
  
}

case class EmptyCalendar() extends Calendar {
  
  override def name:String = "Empty"

  override def isWeekend(w:Weekday):Boolean = w == Saturday || w == Sunday

  override def isBusinessDay(date:qlDate):Boolean = !isWeekend(date.weekday)
  
}

