package squantlib.database.schemadefinitions

import java.util.Date
import java.util.Formatter
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.annotations.Column
import org.squeryl.dsl.{ManyToOne, ManyToMany, OneToMany}
import org.squeryl.KeyedEntity
import squantlib.database.DB
import squantlib.database.objectconstructor._

class Country(@Column("ID")				var id: String,
              @Column("CurrencyID")		var currencyid: String,
              @Column("NAME_JPN")		var name_jpn: String,
              @Column("NAME_ENG")		var name_eng: String,
              @Column("REGION")			var region: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("ADDRESS_LAT")	var address_lat: Option[Double],
              @Column("ADDRESS_LNG")	var address_lng: Option[Double],
              @Column("Created")	var created: Option[Date],
              @Column("LastModified")	var lastmodified: Option[Date]
              ) extends KeyedEntity[String] {

  def this() = this(
    id = null,
    currencyid = null,
    name_jpn = null,
    name_eng = null,
    region = null,
    description_jpn = null,
    description_eng = null,
    address_lat = Some(0.0),
    address_lng = Some(0.0),
    created = Some(new Date),
    lastmodified = Some(new Date)
  )

  override def toString():String = format("%-5s %-15s %-15s %-15s %-15s", id, name_eng, name_jpn, region, created.toString)
}

class Currency(@Column("ID")				var id: String,
              @Column("NAME_JPN")			var name_jpn: String,
              @Column("NAME_JPN_SHORT")		var name_jpn_short: String,
              @Column("NAME_ENG")			var name_eng: String,
              @Column("NAME_ENG_SHORT")		var name_eng_short: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("Created")			var created: Option[Date],
              @Column("LastModified")		var lastmodified : Option[Date]
              ) extends KeyedEntity[String] {
  
  def this() = this(
      id = null, 
      name_jpn = null, 
      name_jpn_short = null, 
      name_eng = null, 
      name_eng_short = null, 
      description_jpn = null, 
      description_eng = null, 
      created = Some(new Date), 
      lastmodified = Some(new Date))

  override def toString():String = format("%-5s %-15s %-15s %-15s", id, name_eng, name_jpn, created.toString)
}

class Distributor(@Column("ID")				var id: String,
              @Column("NAME_JPN")			var name_jpn: String,
              @Column("NAME_JPN_SHORT")		var name_jpn_short: String,
              @Column("NAME_ENG")			var name_eng: String,
              @Column("NAME_ENG_SHORT")		var name_eng_short: String,
              @Column("ADDRESS")			var address: String,
              @Column("POST")				var post: String,
              @Column("TEL")				var tel: String,
              @Column("ADDRESS_LAT")		var address_lat: Option[Double],
              @Column("ADDRESS_LNG")		var address_lng: Option[Double],
              @Column("FSA_REGION")			var fsa_region: String,
              @Column("FSA_ID")				var fsa_id: String,
              @Column("FSA_NAME")			var fsa_name: String,
              @Column("FSA_POST")			var fsa_post: String,
              @Column("FSA_ADDRESS")		var fsa_address: String,
              @Column("FSA_TEL")			var fsa_tel: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("Created")			var created: Option[Date],
              @Column("LastModified")		var lastmodified : Option[Date]
              ) extends KeyedEntity[String] {
  
  def this() = this(
	id = null,
	name_jpn = null,
	name_jpn_short = null,
	name_eng = null,
	name_eng_short = null,
	address = null,
	post = null,
	tel = null,
	address_lat = Some(0.0),
	address_lng = Some(0.0),
	fsa_region = null,
	fsa_id = null,
	fsa_name = null,
	fsa_post = null,
	fsa_address = null,
	fsa_tel = null,
	description_jpn = null,
	description_eng = null,
	created = Some(new Date),
	lastmodified  = Some(new Date))

  override def toString():String = format("%-5s %-15s %-10s %-30s %-15s %-15s", id, name_jpn, post, address, tel, created.toString)
}



class Issuer(@Column("ID")					var id: String,
              @Column("NAME")				var name: String,
              @Column("NAME_JPN")			var name_jpn: String,
              @Column("NAME_JPN_SHORT")		var name_jpn_short: String,
              @Column("ADDRESS")			var address: String,
              @Column("ADDRESS_LAT")		var address_lat: Option[Double],
              @Column("ADDRESS_LNG")		var address_lng: Option[Double],
              @Column("CountryID")			var countryid: String,
              @Column("TYPE")				var issuertype: String,
              @Column("RATING_MDY")			var rating_mdy: Option[Int],
              @Column("RATING_MDY_WATCH")	var rating_mdy_watch: String,
              @Column("RATING_SP")			var rating_SP: Option[Int],
              @Column("RATING_SP_WATCH")	var rating_SP_watch: String,
              @Column("RATING_FITCH")		var rating_FITCH: Option[Int],
              @Column("RATING_FITCH_WATCH")	var rating_FITCH_watch: String,
              @Column("BBG_EQTY")			var bbg_eqty: String,
              @Column("BBG_DEBT")			var bbg_debt: String,
              @Column("EDINET")				var edinet: String,
              @Column("EDINET_NAME")		var edinet_name: String,
              @Column("EDINET_ADDRESS")		var edinet_address: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("Created")			var created: Option[Date],
              @Column("LastModified")		var lastmodified : Option[Date]
              ) extends KeyedEntity[String] {
  
  def this() = this(
		id = null,
		name = null,
		name_jpn = null,
		name_jpn_short = null,
		address = null,
		address_lat = Some(0.0),
		address_lng = Some(0.0),
		countryid = null,
		issuertype = null,
		rating_mdy = Some(0),
		rating_mdy_watch = null,
		rating_SP = Some(0),
		rating_SP_watch = null,
		rating_FITCH = Some(0),
		rating_FITCH_watch = null,
		bbg_eqty = null,
		bbg_debt = null,
		edinet = null,
		edinet_name = null,
		edinet_address = null,
		description_jpn = null,
		description_eng = null,
		created = Some(new Date),
		lastmodified  = Some(new Date))

  override def toString():String = format("%-5s %-15s %-25s %-10s %-15s %-15s", id, name, name_jpn, address, issuertype, created.toString)
}


class Product(@Column("ID")					var id: String,
              @Column("NAME_JPN")			var name_jpn: String,
              @Column("NAME_JPN_SHORT")		var name_jpn_short: String,
              @Column("NAME_ENG")			var name_eng: String,
              @Column("NAME_ENG_SHORT")		var name_eng_short: String,
              @Column("TYPE")				var producttype: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("Created")			var created: Option[Date],
              @Column("LastModified")		var lastmodified : Option[Date]
              ) extends KeyedEntity[String] {
  
  def this() = this(
		id = null,
		name_jpn = null,
		name_jpn_short = null,
		name_eng = null,
		name_eng_short = null,
		producttype = null,
		description_jpn = null,
		description_eng = null,
		created = Some(new Date),
		lastmodified  = Some(new Date))

  override def toString():String = format("%-5s %-15s %-25s %-10s %-15s %-15s", id, name_eng, name_jpn, producttype, producttype, created.toString)
}


class Bond(@Column("ID")					var id: String,
              @Column("REF_NUMBER")			var ref_number: Int,
              @Column("FILING")				var filing: Date,
              @Column("ISSUEDATE")			var issuedate: Date,
              @Column("MATURITY")			var maturity: Date,
              @Column("NOMINAL")			var nominal: Double,
              @Column("DENOMINATION")		var denomination: Option[Double],
              @Column("COUPON")				var coupon: String,
              @Column("COUPON_FREQ")		var coupon_freq: Option[Int],
              @Column("DAYCOUNT")			var daycount: String,
              @Column("DAYCOUNT_ADJ")		var daycount_adj: String,
              @Column("PAYMENT_ADJ")		var payment_adj: String,
              @Column("CALENDAR")			var calendar_str: String,
              @Column("INARREARS")			var inarrears: Option[Int],
              @Column("CPNNOTICE")			var cpnnotice: Option[Int],
              @Column("ISSUEPRICE")			var issueprice: Option[Double],
              @Column("REDEMPRICE")			var redemprice: String,
              @Column("CALL")				var call: String,
              @Column("TYPE")				var bondtype: String,
              @Column("INITIALFX")			var initialfx: Double,
              @Column("ISIN")				var isin: String,
              @Column("TICKER")				var ticker: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("CurrencyID")			var currencyid: String,
              @Column("ProductID")			var productid: String,
              @Column("IssuerID")			var issuerid: String,
              @Column("Created")			var created: Option[Date],
              @Column("LastModified")		var lastmodified : Option[Date]
              ) extends KeyedEntity[String] {

  import org.jquantlib.time.Calendar
  import org.jquantlib.time.calendars.JointCalendar
  import squantlib.model.currencies.CurrencyConversion
  
  def calendar:Calendar = {
    val cdrlist = calendar_str.split(",").map(s => s.trim)
    if (cdrlist.size == 1) CurrencyConversion.getcalendar(cdrlist.head)
    else new JointCalendar(cdrlist.map(c => CurrencyConversion.getcalendar(c)))
  }
  
  def this() = this(
		id = null,
		ref_number = 0,
		filing = new Date,
		issuedate = new Date,
		maturity = new Date,
		nominal = 0.0,
		denomination = Some(0.0),
		coupon = null,
		coupon_freq = Some(0),
		daycount = null,
		daycount_adj = null,
		calendar_str = null,
		payment_adj = null,
		inarrears = Some(0),
		cpnnotice = Some(0),
		issueprice = Some(0.0),
		redemprice = null,
		call = null,
		bondtype = null,
		initialfx = 0.0,
		isin = null,
		ticker = null,
		description_jpn = null,
		description_eng = null,
		currencyid = null,
		productid = null,
		issuerid = null,
		created = Some(new Date),
		lastmodified  = Some(new Date))

  override def toString():String = format("%-5s %-15s %-25s %-10s %-15s %-15s", id, issuedate.toString, maturity.toString, coupon, initialfx.toString, created.toString)
  
  def toFixedRateBond = FixedRateBondConstructor.getbond(this)
  def getCoupons = CouponConstructor.getCoupons(this)
}


class InputParameter(@Column("ID")			var id: Int,
              @Column("PARAMSET")			var paramset: String,
              @Column("PARAMDATE")			var paramdate: Date,
              @Column("INSTRUMENT")			var instrument: String,
              @Column("ASSET")				var asset: String,
              @Column("MATURITY")			var maturity: String,
              @Column("VALUE")				var value: Double,
              @Column("OPTION")				var option: String,
              @Column("COMMENT")			var comment: String,
              @Column("Created")			var created: Option[Date]
              ) extends KeyedEntity[Int] {
  
  def this() = this(
      id = -99999, 
      paramset = null, 
      paramdate = new Date, 
      instrument = null, 
      asset = null, 
      maturity = null, 
      value = -99999, 
      option = null, 
      comment = null, 
      created = Some(new Date))

  override def toString():String = format("%-5s %-15s %-15s %-15s %-15s %-15s", id, paramset, instrument, asset, maturity, value)
}

class CDSParameter(@Column("ID")			var id: Int,
              @Column("PARAMSET")			var paramset: String,
              @Column("PARAMDATE")			var paramdate: Date,
              @Column("INSTRUMENT")			var instrument: String,
              @Column("IssuerID")			var issuerid: String,
              @Column("CurrencyID")			var currencyid: String,
              @Column("SPREAD")				var spread: Double,
              @Column("MATURITY")			var maturity: String,
              @Column("COMMENT")			var comment: String,
              @Column("Created")			var created: Option[Date]
              ) extends KeyedEntity[Int] {
  
  def this() = this(
      id = -99999, 
      paramset = null, 
      paramdate = new Date, 
      instrument = null, 
      issuerid = null, 
      currencyid = null, 
      spread = -99999.0, 
      maturity = null, 
      comment = null, 
      created = Some(new Date))

  override def toString():String = format("%-5s %-15s %-15s %-15s %-15s %-15s", id, paramset, instrument, issuerid, currencyid, spread)
  
  def toCDSCurve = CDSCurveConstructor.getcurve(this)
}


class BondPrice(@Column("ID")			var id:String, 
			@Column("BondID")			var bondid:String,
			@Column("CurrencyID")		var currencyid:String,
			@Column("UnderlyingID")		var underlyingid:String,
			@Column("COMMENT")			var comment:String,
			@Column("PARAMSET")			var paramset:String,
			@Column("PARAMDATE")		var paramdate:Date,
			@Column("FXJPY")			var fxjpy:Double,
			@Column("PRICEDIRTY")		var pricedirty:Double,
			@Column("PRICECLEAN")		var priceclean:Option[Double],
			@Column("ACCRUED")			var accrued:Option[Double],
			@Column("PRICEDIRTY_JPY")	var pricedirty_jpy:Option[Double],
			@Column("PRICECLEAN_JPY")	var priceclean_jpy:Option[Double],
			@Column("ACCRUED_JPY")		var accrued_jpy:Option[Double],
			@Column("YIELD_CONTINUOUS")	var yield_continuous:Option[Double],
			@Column("YIELD_ANNUAL")		var yield_annual:Option[Double],
			@Column("YIELD_SEMIANNUAL")	var yield_semiannual:Option[Double],
			@Column("YIELD_SIMPLE")		var yield_simple:Option[Double],
			@Column("INSTRUMENT")		var instrument:String,
			@Column("BPVALUE")			var bpvalue:Option[Double],
			@Column("ATMRATE")			var atmrate:Option[Double],
			@Column("IRR")				var irr:Option[Double],
			@Column("CURRENTRATE")		var currentrate:Option[Double],
			@Column("NEXTAMOUNT")		var nextamount:Option[Double],
			@Column("NEXTDATE")			var nextdate:Option[Date],
			@Column("DUR_SIMPLE")		var dur_simple:Option[Double],
			@Column("DUR_MODIFIED")		var dur_modified:Option[Double],
			@Column("DUR_MACAULEY")		var dur_macauley:Option[Double],
			@Column("YIELDVALUE")		var yieldvaluebp:Option[Double],
			@Column("CONVEXITY")		var convexity:Option[Double], 
			@Column("Created")			var created:Option[Date],
			@Column("LastModified")		var lastmodified:Option[Date]
			) extends KeyedEntity[String]{
  
  def this() = this(
		id = null,
		bondid = null,
		currencyid = null,
		underlyingid = null,
		comment = null,
		paramset = null,
		paramdate = new Date,
		fxjpy = 0.0,
		pricedirty = 0.0,
		priceclean = Some(0.0),
		accrued = Some(0.0),
		pricedirty_jpy = Some(0.0),
		priceclean_jpy = Some(0.0),
		accrued_jpy = Some(0.0),
		yield_continuous = Some(0.0),
		yield_annual = Some(0.0),
		yield_semiannual = Some(0.0),
		yield_simple = Some(0.0),
		instrument = null,
		bpvalue = Some(0.0),
		atmrate = Some(0.0),
		irr = Some(0.0),
		currentrate = Some(0.0),
		nextamount = Some(0.0),
		nextdate = Some(new Date),
		dur_simple = Some(0.0),
		dur_modified= Some(0.0),
		dur_macauley= Some(0.0),
		yieldvaluebp = Some(0.0),
		convexity = Some(0.0), 		
		created = Some(new Date),
		lastmodified = Some(new Date)
      )
      
  override def toString():String = format("%-15s %-5s %-10s %-15s %-10s", id, currencyid, pricedirty, paramset, if (accrued.isEmpty) "" else accrued.get)
      
}

class FXRate(@Column("ID")					var id:Int,
              @Column("PARAMDATE")			var paramdate:Date,
              @Column("PARAMSET")			var paramset:String,
              @Column("CurrencyID")				var currencyid:String,
              @Column("FXRATE_JPY")			var fxjpy:Double,
              @Column("LastModified")		var lastmodified:Option[Date]
              ) extends KeyedEntity[Int] {
  
  def this() = this(
      id = -99999, 
      paramdate = new Date, 
      paramset = null,
      currencyid = null, 
      fxjpy = -99999.0, 
      lastmodified = Some(new Date))
      
  def toInputParameter:InputParameter = new InputParameter(
      id = -id, 
      paramset = paramset, 
      paramdate = paramdate, 
      instrument = "FX", 
      asset = currencyid, 
      maturity = null, 
      value = fxjpy, 
      option = null, 
      comment = null, 
      created = Some(new Date))

  override def toString():String = format("%-5s %-15s %-15s %-15s %-15s", id, paramdate, paramset, currencyid, fxjpy)
}

class Volatility(@Column("ID")				var id:String,
              @Column("UnderlyingID")		var underlying:String,
              @Column("ValueDate")			var valuedate:Date,
              @Column("Periodicity")		var periodicity:Int,
              @Column("NbDays")				var nbdays:Int,
              @Column("Value")				var value:Double,
              @Column("LastModified")		var lastmodified:Option[Date]
              ) extends KeyedEntity[String] {
  
  def this() = this(
      id = null, 
      underlying = null,
      valuedate = null,
      periodicity = -999999,
      nbdays = -99999,
      value = -99999999,
      lastmodified = Some(new Date))

  override def toString():String = format("%-5s %-15s %-15s %-15s %-15s", id, underlying, valuedate, periodicity, value)
}

class Correlation(@Column("ID")				var id:String,
              @Column("UnderlyingID")		var underlying1:String,
              @Column("Underlying2ID")		var underlying2:String,
              @Column("ValueDate")			var valuedate:Date,
              @Column("Periodicity")		var periodicity:Int,
              @Column("NbDays")				var nbdays:Int,
              @Column("Value")				var value:Double,
              @Column("LastModified")		var lastmodified:Option[Date]
              ) extends KeyedEntity[String] {
  
  def this() = this(
      id = null, 
      underlying1 = null,
      underlying2 = null,
      valuedate = null,
      periodicity = -999999,
      nbdays = -99999,
      value = -99999999,
      lastmodified = Some(new Date))

  override def toString():String = format("%-5s %-15s %-15s %-15s %-15s", id, underlying1, underlying2, valuedate, value)
}

class Coupon(@Column("ID")				var id:String,
			@Column("BondID")			var bondid:String,
			@Column("CurrencyID")		var currency:String,
			@Column("Rate")				var rate:String,
			@Column("EventDate")		var eventdate:Date,
			@Column("StartDate")		var startdate:Date,
			@Column("EndDate")			var enddate:Date,
			@Column("PaymentDate")		var paymentdate:Date,
			@Column("FixedAmount")		var fixedamount:Option[Double],
			@Column("Comment")			var comment:String,
			@Column("Daycount")			var daycount:String,
			@Column("PaymentType")		var paymenttype:String,
			@Column("LastModified")		var lastmodified:Option[Date]
              ) extends KeyedEntity[String] {
  
  def this() = this(
      id = null,
      bondid = null,
      currency = null,
      rate = null,
      eventdate = null,
      startdate = null,
      enddate = null,
      paymentdate = null,
      fixedamount = Some(-999.0),
      comment = null,
      daycount = null,
      paymenttype = null,
      lastmodified = Some(new Date))
      
  override def toString():String = format("%-15s %-5s %-15s %tF %tF %tF %tF %-15s", bondid, currency, rate, eventdate, startdate, enddate, paymentdate, fixedamount.getOrElse(""))
      
}

