package net.squantlib.util.initializer

import org.jquantlib.time.calendars._
import org.jquantlib.time.Calendar
import org.jquantlib.currencies.{Currency => qlCurrency}
import net.squantlib.util.{DbCalendar, EmptyCalendar}
import net.squantlib.database.DB

object Calendars extends Initializer[DbCalendar] {

	def apply(ids:Traversable[String]):Option[DbCalendar] = {
    val cdrlist = ids.filter(keySet.contains)
    cdrlist.size match {
      case 0 => None
      case 1 => Some(mapper(cdrlist.head))
      case _ => 
        val cals = cdrlist.map(c => mapper(c))
        Some(DbCalendar(new JointCalendar(cals.map(_.calendar).toArray), cals.map(_.countryIds).flatten.toSet, DB.lastHoliday.ql))
    }
	}
	
	def empty:Calendar = EmptyCalendar()
	
	def defaultCurrencyCalendars = Map(
    "ARS" -> new Argentina,
    "AUD" -> new Australia,
    "BRL" -> new Brazil,
    "CAD" -> new Canada,
    "CNY" -> new China,
    "CZK" -> new CzechRepublic,
    "DKK" -> new Denmark,
    "HKD" -> new HongKong,
    "HUF" -> new Hungary,
    "ISK" -> new Iceland,
    "INR" -> new India,
    "IDR" -> new Indonesia,
    "JPY" -> new Japan,
    "MXN" -> new Mexico,
    "NZD" -> new NewZealand,
    "NOK" -> new Norway,
    "PLN" -> new Poland,
    "RON" -> new Romania,
    "RUB" -> new Russia,
    "SAR" -> new SaudiArabia,
    "SGD" -> new Singapore,
    "ZAR" -> new SouthAfrica,
    "KRW" -> new SouthKorea,
    "SEK" -> new Sweden,
    "CHF" -> new Switzerland,
    "TWD" -> new Taiwan,
    "EUR" -> new Target,
    "TRY" -> new Turkey,
    "GBP" -> new UnitedKingdom,
    "USD" -> new UnitedStates
	)
	
  def currencyMapper:Map[String, DbCalendar] = DB.getCurrencyHolidayMapping.map{case (currencyId, holidayId) =>
    (currencyId, DbCalendar(defaultCountryCalendars.getOrElse(currencyId, new NullCalendar), Set(holidayId), DB.lastHoliday.ql))
  }

  
	def defaultCountryCalendars = Map(
    "ARG" -> new Argentina,
    "AUS" -> new Australia,
    "BRA" -> new Brazil,
    "CAN" -> new Canada,
    "CHN" -> new China,
    "CZE" -> new CzechRepublic,
    "DNK" -> new Denmark,
    "HKG" -> new HongKong,
    "HUN" -> new Hungary,
    "ISL" -> new Iceland,
    "IND" -> new India,
    "IDN" -> new Indonesia,
    "JPN" -> new Japan,
    "MEX" -> new Mexico,
    "NZL" -> new NewZealand,
    "NOR" -> new Norway,
    "POL" -> new Poland,
    "ROU" -> new Romania,
    "RUS" -> new Russia,
    "SAU" -> new SaudiArabia,
    "SGP" -> new Singapore,
    "ZAF" -> new SouthAfrica,
    "KOR" -> new SouthKorea,
    "SWE" -> new Sweden,
    "CHE" -> new Switzerland,
    "TWN" -> new Taiwan,
    "EUR" -> new Target,
    "TUR" -> new Turkey,
    "GBR" -> new UnitedKingdom,
    "USA" -> new UnitedStates
	)
	
  def countryToDbCalendar(countryId:String, defaultCalendar:Calendar):Option[(String, DbCalendar)] = 
    DB.getCountryHolidayMapping.get(countryId).collect{case holidayId => (countryId, DbCalendar(defaultCalendar, Set(holidayId), DB.lastHoliday.ql))}

  def countryMapper:Map[String, DbCalendar] = DB.getCountryHolidayMapping.map{case (countryId, holidayId) =>
    (countryId, DbCalendar(defaultCountryCalendars.getOrElse(holidayId, new NullCalendar), Set(holidayId), DB.lastHoliday.ql))
  }
  
  val mapper:Map[String, DbCalendar] = countryMapper ++ currencyMapper		
}


