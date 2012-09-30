package squantlib.initializer

import org.jquantlib.time.calendars._
import org.jquantlib.time.Calendar
import org.jquantlib.currencies.{Currency => qlCurrency}

object Calendars extends Initializer[Calendar] {

	def apply(ids:Traversable[String]):Option[Calendar] = {
	  val cdrlist = ids.filter(keySet.contains)
	  cdrlist.size match {
	    case 0 => None
	    case 1 => apply(cdrlist.head)
	    case _ => Some(new JointCalendar(cdrlist.map(c => apply(c).get).toArray))
	  }
	}
	
	val mapper = Map(
		("ARS" -> new Argentina),
		("AUD" -> new Australia),
		("BRL" -> new Brazil),
		("CAD" -> new Canada),
		("CNY" -> new China),
		("CZK" -> new CzechRepublic),
		("DKK" -> new Denmark),
		("HKD" -> new HongKong),
		("HUF" -> new Hungary),
		("ISK" -> new Iceland),
		("INR" -> new India),
		("IDR" -> new Indonesia),
		("JPY" -> new Japan),
		("MXN" -> new Mexico),
		("NZD" -> new NewZealand), 
		("NOK" -> new Norway),
		("PLN" -> new Poland),
		("RON" -> new Romania),
		("RUB" -> new Russia),
		("SAR" -> new SaudiArabia),
		("SGD" -> new Singapore),
		("ZAR" -> new SouthAfrica),
		("KRW" -> new SouthKorea),
		("SEK" -> new Sweden),
		("CHF" -> new Switzerland),
		("TWD" -> new Taiwan),
		("EUR" -> new Target),
		("TRY" -> new Turkey),
		("GBP" -> new UnitedKingdom),
		("USD" -> new UnitedStates))

}


