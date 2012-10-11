package squantlib.database.external

import java.math.{BigDecimal => JavaBigDecimal}
import java.net.URL
import java.net.URLConnection
import java.util.Calendar
import java.util.{Map => JavaMap, Date => JavaDate}
import scala.collection.JavaConversions._

import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper

/** 
 * Implements Json for Open Exchange Rates(http://openexchangerates.org)
 * 
 * @author Masakatsu Wakayu (direct translation of Java codes by Demétrio Menezes Neto)
 */
object OpenExchangeRates {
	val OER_URL = "http://openexchangerates.org/"
	val LATEST = "latest.json"
	val HISTORICAL:String = "historical/%04d-%02d-%02d.json"

	val mapper:ObjectMapper = new ObjectMapper

	/**
	 * Downloads the exchanges rates from given json path
	 * 
	 * @param downloadPath
	 *            Path to json file
	 * @return Map containing all rates of json file
	 * @throws UnavailableExchangeRateException
	 */ 
	
	def downloadExchangeRates(downloadPath:String):Map[String, BigDecimal] 
	= downloadExchangeRates(downloadPath, CurrencyList.get)
	
	def downloadExchangeRates(downloadPath:String, currencies:Set[String]):Map[String, BigDecimal] = {
		try {
			val url:URL = new URL(OER_URL + downloadPath)
			val conn:URLConnection = url.openConnection
			val node:JsonNode = mapper.readTree(conn.getInputStream)
			val rates:Set[Option[(String, BigDecimal)]] = currencies.map { ccy => {
				node.findValue(ccy) match {
				  case null => None
				  case c => Some(ccy, BigDecimal(c.getDecimalValue))
				}
			}}
			rates.flatten.toMap
		}
		catch {
		  	case e:Exception => { e.printStackTrace; Map.empty }
		}
	}

	/**
	 * Get the latest exchange rates
	 * 
	 * @return Last updated exchange rates
	 */
	def getLatest:Map[String, BigDecimal] = downloadExchangeRates(LATEST)
	
	def getHistorical(date:JavaDate):Map[String, BigDecimal] = {
	  val cal = Calendar.getInstance
	  cal.setTime(date)
	  getHistorical(cal)
	}

	def getHistorical(date:Calendar):Map[String, BigDecimal] = {
		val day = date.get(Calendar.DAY_OF_MONTH)
		val month = date.get(Calendar.MONTH) + 1
		val year = date.get(Calendar.YEAR)

		val historical = HISTORICAL.format(year, month, day)
		downloadExchangeRates(historical)

	}

	def getCurrencyValue(currency:String):Option[BigDecimal] = getLatest.get(currency)

	def getHistoricalCurrencyValue(currency:String, date:Calendar):Option[BigDecimal] = 
		getHistorical(date).get(currency)
	
}



object CurrencyList { 
  
  val get = Set(
	"AED",   // United Arab Emirates Dirham
	"AFN",   // Afghan Afghani
	"ALL",    // Albanian Lek
	"AMD",    // Armenian Dram
	"ANG",    // Netherlands Antillean Guilder
	"AOA",    // Angolan Kwanza
	"ARS",    // Argentine Peso
	"AUD",    // Australian Dollar
	"AWG",    // Aruban Florin
	"AZN",    // Azerbaijani Manat
	"BAM",    // Bosnia-Herzegovina Convertible Mark
	"BBD",    // Barbadian Dollar
	"BDT",    // Bangladeshi Taka
	"BGN",    // Bulgarian Lev
	"BHD",    // Bahraini Dinar
	"BIF",    // Burundian Franc
	"BMD",    // Bermudan Dollar
	"BND",    // Brunei Dollar
	"BOB",    // Bolivian Boliviano
	"BRL",    // Brazilian Real
	"BSD",    // Bahamian Dollar
	"BTN",    // Bhutanese Ngultrum
	"BWP",    // Botswanan Pula
	"BYR",    // Belarusian Ruble
	"BZD",    // Belize Dollar
	"CAD",    // Canadian Dollar
	"CDF",    // Congolese Franc
	"CHF",    // Swiss Franc
	"CLF",    // Chilean Unit of Account (UF)
	"CLP",    // Chilean Peso
	"CNY",    // Chinese Yuan
	"COP",    // Colombian Peso
	"CRC",    // Costa Rican Colón
	"CUP",    // Cuban Peso
	"CVE",    // Cape Verdean Escudo
	"CZK",    // Czech Republic Koruna
	"DJF",    // Djiboutian Franc
	"DKK",    // Danish Krone
	"DOP",    // Dominican Peso
	"DZD",    // Algerian Dinar
	"EGP",    // Egyptian Pound
	"ETB",    // Ethiopian Birr
	"EUR",    // Euro
	"FJD",    // Fijian Dollar
	"FKP",    // Falkland Islands Pound
	"GBP",    // British Pound Sterling
	"GEL",    // Georgian Lari
	"GHS",    // Ghanaian Cedi
	"GIP",    // Gibraltar Pound
	"GMD",    // Gambian Dalasi
	"GNF",    // Guinean Franc
	"GTQ",    // Guatemalan Quetzal
	"GYD",    // Guyanaese Dollar
	"HKD",    // Hong Kong Dollar
	"HNL",    // Honduran Lempira
	"HRK",    // Croatian Kuna
	"HTG",    // Haitian Gourde
	"HUF",    // Hungarian Forint
	"IDR",    // Indonesian Rupiah
	"IEP",    // Irish Pound
	"ILS",    // Israeli New Sheqel
	"INR",    // Indian Rupee
	"IQD",    // Iraqi Dinar
	"IRR",    // Iranian Rial
	"ISK",    // Icelandic Króna
	"JMD",    // Jamaican Dollar
	"JOD",    // Jordanian Dinar
	"JPY",    // Japanese Yen
	"KES",    // Kenyan Shilling
	"KGS",    // Kyrgystani Som
	"KHR",    // Cambodian Riel
	"KMF",    // Comorian Franc
	"KPW",    // North Korean Won
	"KRW",    // South Korean Won
	"KWD",    // Kuwaiti Dinar
	"KZT",    // Kazakhstani Tenge
	"LAK",    // Laotian Kip
	"LBP",    // Lebanese Pound
	"LKR",    // Sri Lankan Rupee
	"LRD",    // Liberian Dollar
	"LSL",    // Lesotho Loti
	"LTL",    // Lithuanian Litas
	"LVL",    // Latvian Lats
	"LYD",    // Libyan Dinar
	"MAD",    // Moroccan Dirham
	"MDL",    // Moldovan Leu
	"MGA",    // Malagasy Ariary
	"MKD",    // Macedonian Denar
	"MMK",    // Myanma Kyat
	"MNT",    // Mongolian Tugrik
	"MOP",    // Macanese Pataca
	"MRO",    // Mauritanian Ouguiya
	"MUR",    // Mauritian Rupee
	"MVR",    // Maldivian Rufiyaa
	"MWK",    // Malawian Kwacha
	"MXN",    // Mexican Peso
	"MYR",    // Malaysian Ringgit
	"MZN",    // Mozambican Metical
	"NAD",    // Namibian Dollar
	"NGN",    // Nigerian Naira
	"NIO",    // Nicaraguan Córdoba
	"NOK",    // Norwegian Krone
	"NPR",    // Nepalese Rupee
	"NZD",    // New Zealand Dollar
	"OMR",    // Omani Rial
	"PAB",    // Panamanian Balboa
	"PEN",    // Peruvian Nuevo Sol
	"PGK",    // Papua New Guinean Kina
	"PHP",    // Philippine Peso
	"PKR",    // Pakistani Rupee
	"PLN",    // Polish Zloty
	"PYG",    // Paraguayan Guarani
	"QAR",    // Qatari Rial
	"RON",    // Romanian Leu
	"RSD",    // Serbian Dinar
	"RUB",    // Russian Ruble
	"RWF",    // Rwandan Franc
	"SAR",    // Saudi Riyal
	"SBD",    // Solomon Islands Dollar
	"SCR",    // Seychellois Rupee
	"SDG",    // Sudanese Pound
	"SEK",    // Swedish Krona
	"SGD",    // Singapore Dollar
	"SHP",    // Saint Helena Pound
	"SLL",    // Sierra Leonean Leone
	"SOS",    // Somali Shilling
	"SRD",    // Surinamese Dollar
	"STD",    // São Tomé and Príncipe Dobra
	"SVC",    // Salvadoran Colón
	"SYP",    // Syrian Pound
	"SZL",    // Swazi Lilangeni
	"THB",    // Thai Baht
	"TJS",    // Tajikistani Somoni
	"TMT",    // Turkmenistani Manat
	"TND",    // Tunisian Dinar
	"TOP",    // Tongan Paʻanga
	"TRY",    // Turkish Lira
	"TTD",    // Trinidad and Tobago Dollar
	"TWD",    // New Taiwan Dollar
	"TZS",    // Tanzanian Shilling
	"UAH",    // Ukrainian Hryvnia
	"UGX",    // Ugandan Shilling
	"USD",    // United States Dollar
	"UYU",    // Uruguayan Peso
	"UZS",    // Uzbekistan Som
	"VEF",    // Venezuelan Bolívar
	"VND",    // Vietnamese Dong
	"VUV",    // Vanuatu Vatu
	"WST",    // Samoan Tala
	"XAF",    // CFA Franc BEAC
	"XCD",    // East Caribbean Dollar
	"XDR",    // Special Drawing Rights
	"XOF",    // CFA Franc BCEAO
	"XPF",    // CFP Franc
	"YER",    // Yemeni Rial
	"ZAR",    // South African Rand
	"ZMK",    // Zambian Kwacha
	"ZWD",    // Zimbabwean Dollar (1980-2008)
	"ZWL")    // Zimbabwean Dollar
  
}