package net.squantlib.util.initializer

import net.squantlib.util.ql.time.calendars._
import net.squantlib.util.ql.time.Calendar
import net.squantlib.util.ql.currencies.{Currency => qlCurrency}
import net.squantlib.util.{DbCalendar, EmptyCalendar}
import net.squantlib.database.DB

object Calendars extends Initializer[DbCalendar] {

	def apply(ids:Traversable[String]):Option[DbCalendar] = {
    Some(DbCalendar(ids.toSet))
//    val cdrlist = ids.filter(keySet.contains)
//    cdrlist.size match {
//      case 0 => None
//      case 1 => Some(mapper(cdrlist.head))
//      case _ =>
//        val cals = cdrlist.map(c => mapper(c))
//        Some(DbCalendar(new JointCalendar(cals.map(_.calendar).toArray), cals.map(_.countryIds).flatten.toSet, DB.lastHoliday.ql))
//    }
	}

  override def apply(id:String):Option[DbCalendar] = Some(DbCalendar(Set(id)))

  val currencyCountryMapping:Map[String, String] = Map(
    "AED" -> "UAE",
    "ARS" -> "ARG",
    "AUD" -> "SYD",
    "BRL" -> "BRA",
    "CAD" -> "YTO",
    "CHF" -> "ZRH",
    "CLP" -> "CHL",
    "CNY" -> "CHN",
    "CZK" -> "PRG",
    "DKK" -> "CPH",
    "EGP" -> "EGY",
    "EUR" -> "EUR",
    "GBP" -> "LDN",
    "HKD" -> "HKG",
    "HRK" -> "HRV",
    "HUF" -> "BUD",
    "IDR" -> "JKT",
    "ILS" -> "ISR",
    "INR" -> "BOM",
    "ISK" -> "ISL",
    "JPY" -> "TKY",
    "KRW" -> "SEL",
    "KWD" -> "KWT",
    "MAD" -> "MAR",
    "MXN" -> "MXC",
    "MYR" -> "MYS",
    "NGN" -> "NGA",
    "NOK" -> "NOR",
    "NZD" -> "WLG",
    "OMR" -> "OMN",
    "PEN" -> "PER",
    "PHP" -> "PHL",
    "PLN" -> "WAW",
    "PYG" -> "PRY",
    "RON" -> "BUH",
    "RUB" -> "MOW",
    "SAR" -> "SAU",
    "SEK" -> "SWE",
    "SGD" -> "SGP",
    "THB" -> "THA",
    "TND" -> "TUN",
    "TRY" -> "IST",
    "TWD" -> "TWN",
    "UAH" -> "UKR",
    "USD" -> "NYC",
    "UYU" -> "URY",
    "VEF" -> "VEN",
    "VND" -> "VNM",
    "ZAR" -> "JNB"
  )


  val mapper:Map[String, DbCalendar] = {
    val countryMappers:Map[String, DbCalendar] = DB.getCountryIds.map { case (cid) =>
      (cid, DbCalendar(Set(cid)))
    }.toMap

    val currencyMappers:Map[String, DbCalendar] = currencyCountryMapping.filter{case (k, v) => !countryMappers.contains(k)}.map{case (cid, hid) =>
      (cid, DbCalendar(Set(hid)))
    }.toMap

    countryMappers ++ currencyMappers
  }

	def empty:DbCalendar = EmptyCalendar()

//
//	def defaultCurrencyCalendars = Map(
//    "ARS" -> new Argentina,
//    "AUD" -> new Australia,
//    "BRL" -> new Brazil,
//    "CAD" -> new Canada,
//    "CNY" -> new China,
//    "CZK" -> new CzechRepublic,
//    "DKK" -> new Denmark,
//    "HKD" -> new HongKong,
//    "HUF" -> new Hungary,
//    "ISK" -> new Iceland,
//    "INR" -> new India,
//    "IDR" -> new Indonesia,
//    "JPY" -> new Japan,
//    "MXN" -> new Mexico,
//    "NZD" -> new NewZealand,
//    "NOK" -> new Norway,
//    "PLN" -> new Poland,
//    "RON" -> new Romania,
//    "RUB" -> new Russia,
//    "SAR" -> new SaudiArabia,
//    "SGD" -> new Singapore,
//    "ZAR" -> new SouthAfrica,
//    "KRW" -> new SouthKorea,
//    "SEK" -> new Sweden,
//    "CHF" -> new Switzerland,
//    "TWD" -> new Taiwan,
//    "EUR" -> new Target,
//    "TRY" -> new Turkey,
//    "GBP" -> new UnitedKingdom,
//    "USD" -> new UnitedStates
//	)
//
//  def currencyMapper:Map[String, DbCalendar] = DB.getCurrencyHolidayMapping.map{case (currencyId, holidayId) =>
//    (currencyId, DbCalendar(defaultCountryCalendars.getOrElse(currencyId, new NullCalendar), Set(holidayId), DB.lastHoliday.ql))
//  }
//
//
//	def defaultCountryCalendars = Map(
//    "ARG" -> new Argentina,
//    "AUS" -> new Australia,
//    "BRA" -> new Brazil,
//    "CAN" -> new Canada,
//    "CHN" -> new China,
//    "CZE" -> new CzechRepublic,
//    "DNK" -> new Denmark,
//    "HKG" -> new HongKong,
//    "HUN" -> new Hungary,
//    "ISL" -> new Iceland,
//    "IND" -> new India,
//    "IDN" -> new Indonesia,
//    "JPN" -> new Japan,
//    "MEX" -> new Mexico,
//    "NZL" -> new NewZealand,
//    "NOR" -> new Norway,
//    "POL" -> new Poland,
//    "ROU" -> new Romania,
//    "RUS" -> new Russia,
//    "SAU" -> new SaudiArabia,
//    "SGP" -> new Singapore,
//    "ZAF" -> new SouthAfrica,
//    "KOR" -> new SouthKorea,
//    "SWE" -> new Sweden,
//    "CHE" -> new Switzerland,
//    "TWN" -> new Taiwan,
//    "EUR" -> new Target,
//    "TUR" -> new Turkey,
//    "GBR" -> new UnitedKingdom,
//    "USA" -> new UnitedStates
//	)
//
//  def countryToDbCalendar(countryId:String, defaultCalendar:Calendar):Option[(String, DbCalendar)] =
//    DB.getCountryHolidayMapping.get(countryId).collect{case holidayId => (countryId, DbCalendar(defaultCalendar, Set(holidayId), DB.lastHoliday.ql))}
//
//  def countryMapper:Map[String, DbCalendar] = DB.getCountryHolidayMapping.map{case (countryId, holidayId) =>
//    (countryId, DbCalendar(defaultCountryCalendars.getOrElse(holidayId, new NullCalendar), Set(holidayId), DB.lastHoliday.ql))
//  }
//
//  val mapper:Map[String, DbCalendar] = countryMapper ++ currencyMapper
}


