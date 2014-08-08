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
	
  val currencyMapper = Map(
    ("ARS" -> DbCalendar(new Argentina, Set("ARG"), DB.lastHoliday.ql)),
    ("AUD" -> DbCalendar(new Australia, Set("AUS"), DB.lastHoliday.ql)),
    ("BRL" -> DbCalendar(new Brazil, Set("BRA"), DB.lastHoliday.ql)),
    ("CAD" -> DbCalendar(new Canada, Set("CAN"), DB.lastHoliday.ql)),
    ("CNY" -> DbCalendar(new China, Set("CHN"), DB.lastHoliday.ql)),
    ("CZK" -> DbCalendar(new CzechRepublic, Set("CZE"), DB.lastHoliday.ql)),
    ("DKK" -> DbCalendar(new Denmark, Set("DNK"), DB.lastHoliday.ql)),
    ("HKD" -> DbCalendar(new HongKong, Set("HKG"), DB.lastHoliday.ql)),
    ("HUF" -> DbCalendar(new Hungary, Set("HUN"), DB.lastHoliday.ql)),
    ("ISK" -> DbCalendar(new Iceland, Set("ISL"), DB.lastHoliday.ql)),
    ("INR" -> DbCalendar(new India, Set("IND"), DB.lastHoliday.ql)),
    ("IDR" -> DbCalendar(new Indonesia, Set("IDN"), DB.lastHoliday.ql)),
    ("JPY" -> DbCalendar(new Japan, Set("JPN"), DB.lastHoliday.ql)),
    ("MXN" -> DbCalendar(new Mexico, Set("MEX"), DB.lastHoliday.ql)),
    ("NZD" -> DbCalendar(new NewZealand, Set("NZL"), DB.lastHoliday.ql)),
    ("NOK" -> DbCalendar(new Norway, Set("NOR"), DB.lastHoliday.ql)),
    ("PLN" -> DbCalendar(new Poland, Set("POL"), DB.lastHoliday.ql)),
    ("RON" -> DbCalendar(new Romania, Set("ROU"), DB.lastHoliday.ql)),
    ("RUB" -> DbCalendar(new Russia, Set("RUS"), DB.lastHoliday.ql)),
    ("SAR" -> DbCalendar(new SaudiArabia, Set("SAU"), DB.lastHoliday.ql)),
    ("SGD" -> DbCalendar(new Singapore, Set("SGP"), DB.lastHoliday.ql)),
    ("ZAR" -> DbCalendar(new SouthAfrica, Set("ZAF"), DB.lastHoliday.ql)),
    ("KRW" -> DbCalendar(new SouthKorea, Set("KOR"), DB.lastHoliday.ql)),
    ("SEK" -> DbCalendar(new Sweden, Set("SWE"), DB.lastHoliday.ql)),
    ("CHF" -> DbCalendar(new Switzerland, Set("CHE"), DB.lastHoliday.ql)),
    ("TWD" -> DbCalendar(new Taiwan, Set("TWN"), DB.lastHoliday.ql)),
    ("EUR" -> DbCalendar(new Target, Set("EUR"), DB.lastHoliday.ql)),
    ("TRY" -> DbCalendar(new Turkey, Set("TUR"), DB.lastHoliday.ql)),
    ("GBP" -> DbCalendar(new UnitedKingdom, Set("GBR"), DB.lastHoliday.ql)),
    ("USD" -> DbCalendar(new UnitedStates, Set("USA"), DB.lastHoliday.ql))
  )

  val countryMapper = Map(
    ("ARG" -> DbCalendar(new Argentina, Set("ARG"), DB.lastHoliday.ql)),
    ("AUS" -> DbCalendar(new Australia, Set("AUS"), DB.lastHoliday.ql)),
    ("BRA" -> DbCalendar(new Brazil, Set("BRA"), DB.lastHoliday.ql)),
    ("CAN" -> DbCalendar(new Canada, Set("CAN"), DB.lastHoliday.ql)),
    ("CHN" -> DbCalendar(new China, Set("CHN"), DB.lastHoliday.ql)),
    ("CZE" -> DbCalendar(new CzechRepublic, Set("CZE"), DB.lastHoliday.ql)),
    ("DNK" -> DbCalendar(new Denmark, Set("DNK"), DB.lastHoliday.ql)),
    ("HKG" -> DbCalendar(new HongKong, Set("HKG"), DB.lastHoliday.ql)),
    ("HUN" -> DbCalendar(new Hungary, Set("HUN"), DB.lastHoliday.ql)),
    ("ISL" -> DbCalendar(new Iceland, Set("ISL"), DB.lastHoliday.ql)),
    ("IND" -> DbCalendar(new India, Set("IND"), DB.lastHoliday.ql)),
    ("IDN" -> DbCalendar(new Indonesia, Set("IDN"), DB.lastHoliday.ql)),
    ("JPN" -> DbCalendar(new Japan, Set("JPN"), DB.lastHoliday.ql)),
    ("MEX" -> DbCalendar(new Mexico, Set("MEX"), DB.lastHoliday.ql)),
    ("NZL" -> DbCalendar(new NewZealand, Set("NZL"), DB.lastHoliday.ql)),
    ("NOR" -> DbCalendar(new Norway, Set("NOR"), DB.lastHoliday.ql)),
    ("POL" -> DbCalendar(new Poland, Set("POL"), DB.lastHoliday.ql)),
    ("ROU" -> DbCalendar(new Romania, Set("ROU"), DB.lastHoliday.ql)),
    ("RUS" -> DbCalendar(new Russia, Set("RUS"), DB.lastHoliday.ql)),
    ("SAU" -> DbCalendar(new SaudiArabia, Set("SAU"), DB.lastHoliday.ql)),
    ("SGP" -> DbCalendar(new Singapore, Set("SGP"), DB.lastHoliday.ql)),
    ("ZAF" -> DbCalendar(new SouthAfrica, Set("ZAF"), DB.lastHoliday.ql)),
    ("KOR" -> DbCalendar(new SouthKorea, Set("KOR"), DB.lastHoliday.ql)),
    ("SWE" -> DbCalendar(new Sweden, Set("SWE"), DB.lastHoliday.ql)),
    ("CHE" -> DbCalendar(new Switzerland, Set("CHE"), DB.lastHoliday.ql)),
    ("TWN" -> DbCalendar(new Taiwan, Set("TWN"), DB.lastHoliday.ql)),
    ("EUR" -> DbCalendar(new Target, Set("EUR"), DB.lastHoliday.ql)),
    ("TUR" -> DbCalendar(new Turkey, Set("TUR"), DB.lastHoliday.ql)),
    ("GBR" -> DbCalendar(new UnitedKingdom, Set("GBR"), DB.lastHoliday.ql)),
    ("USA" -> DbCalendar(new UnitedStates, Set("USA"), DB.lastHoliday.ql))
  )
  
  val mapper:Map[String, DbCalendar] = countryMapper ++ currencyMapper		
}


