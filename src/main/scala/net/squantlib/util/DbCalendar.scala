package net.squantlib.util

import org.jquantlib.time.Calendar
import net.squantlib.database.DB
import org.jquantlib.time.Weekday
import org.jquantlib.time.Weekday._
import org.jquantlib.time.{Date => qlDate}

class DbCalendar(val countryIds:Set[String], val holidayList:Set[qlDate]) extends Calendar {

  val dbLastDate = if (holidayList.isEmpty) null else holidayList.max

  def isDbPeriod(d:qlDate) = if (dbLastDate == null) false else d.le(dbLastDate)
  
  //lazy val dbHolidays:Set[qlDate] = countryIds.map(id => DB.getHolidays(id)).flatten.map(_.ql)
  
  override def name:String = "DB calendar " + countryIds.mkString(" ")

  override def isWeekend(w:Weekday):Boolean = w == Saturday || w == Sunday

  override def isBusinessDay(date:qlDate):Boolean = !isWeekend(date.weekday) && !holidayList.contains(date)
//    if (isDbPeriod(date)) !isWeekend(date.weekday) && !holidayList.contains(date)
//    else calendar.isBusinessDay(date)

}

object DbCalendar {

  def apply(countryIds:Set[String]) = {
    val dbHolidays:Set[qlDate] = countryIds.map(id => DB.getHolidays(id)).flatten.map(_.ql)
    new DbCalendar(countryIds, dbHolidays)
  }
}


case class EmptyCalendar() extends DbCalendar(Set.empty, Set.empty) {
  override def name:String = "Empty"
}

