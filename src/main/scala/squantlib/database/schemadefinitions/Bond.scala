package squantlib.database.schemadefinitions

import java.util.{Date => JavaDate}
import scala.collection.JavaConversions._
import scala.language.postfixOps
import squantlib.util.JsonUtils._
import squantlib.schedule.Schedule
import squantlib.util.initializer._
import squantlib.database.DB
import squantlib.util.{FixingInformation, Date}
import org.jquantlib.time.{Period => qlPeriod, DateGeneration, Calendar, BusinessDayConvention, TimeUnit}
import org.jquantlib.daycounters._
import org.jquantlib.currencies.Currency
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import org.squeryl.annotations.Transient
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity

class Bond(	  @Column("ID")					override var id: String,
              @Column("REF_NUMBER")			var ref_number: Int,
              @Column("FILING")				var filing: JavaDate,
              @Column("SELLSTART")			var sellstart: JavaDate,
              @Column("SELLEND")			var sellend: JavaDate,
              @Column("ISSUEDATE")			var issuedate: JavaDate,
              @Column("MATURITY")			var maturity: JavaDate,
              @Column("TERMINATIONDATE")	var terminationdate: Option[JavaDate],
              @Column("ISMATURED")			var ismatured: Int,
              @Column("NOMINAL")			var nominal: Option[Double],
              @Column("DENOMINATION")		var denomination: Option[Double],
              @Column("UNDERLYING")			var underlying: String,
              @Column("UNDERLYINGINFO")		var underlyinginfo: String,
              @Column("COUPON")				var coupon: String,
              @Column("COUPON_FREQ")		var coupon_freq: Option[Int],
              @Column("COUPON_PERYR")		var coupon_peryr: Int,
              @Column("DAYCOUNT")			var daycount: String,
              @Column("DAYCOUNT_ADJ")		var daycount_adj: String,
              @Column("PAYMENT_ADJ")		var payment_adj: String,
              @Column("CALENDAR")			var calendar_str: String,
              @Column("INARREARS")			var inarrears: Option[Int],
              @Column("CPNNOTICE")			var cpnnotice: Option[Int],
              @Column("ISSUEPRICE")			var issueprice: Option[Double],
              @Column("REDEMPRICE")			var redemprice: String,
              @Column("REDEMNOTICE")		var redemnotice: Option[Int],
              @Column("CALL")				var call: String,
              @Column("TYPE")				var bondtype: String,
              @Column("INITIALFX")			var initialfx: Double,
              @Column("FIXINGDATE")			var fixingdate: Option[JavaDate],
              @Column("FIXINGS")			var fixings: String,
              @Column("ISIN")				var isin: String,
              @Column("TICKER")				var ticker: String,
              @Column("LEADMANAGER")		var leadmanager: String,
              @Column("BONDNAME")			var bondname: String,
              @Column("SHORTNAME")			var shortname: String,
              @Column("EDINETNAME")			var edinetname: String,
              @Column("DESCRIPTION_JPN")	var description_jpn: String,
              @Column("DESCRIPTION_ENG")	var description_eng: String,
              @Column("CurrencyID")			var currencyid: String,
              @Column("ProductID")			var productid: String,
              @Column("IssuerID")			var issuerid: String,
              @Column("RISKTAGS") 			var risktags: String,
              @Column("SETTINGS") 			var settings: String,
              @Column("FIRSTCOUPON")		var firstcoupon: Option[Double],
              @Column("CURRENTCOUPON")		var currentcoupon:Option[Double],
              @Column("TARGETYIELD")		var targetyield: Option[Double],
              @Column("PRICETYPE") 			var pricetype: String,
              @Column("DEFAULT_VOL") 		var defaultvol: Double,
              @Column("COMMENT") 			var comment: String,
              @Column("PAID_IN_JPY") 		var jpypayment: Int,
              @Column("PHYSICAL_REDEMPTION") var physicalredemption: Int,
              @Column("MINIMUM_PURCHASE") 	var minimum_purchase: Option[Double],
              @Column("FIXING_METHOD") 	    var fixing_method: String,
              @Column("MODEL_OUTPUT")      var model_output: String,
              @Column("PricingID")      var pricingid: String,
              @Column("CatchPhrase")      var catchphrase: String,
              @Column("Characteristics")  var characteristics: String,
              @Column("ChartSettings")    var chartsettings: String,
              @Column("Information")      var information: String,
              @Column("Created")			var created: Option[JavaDate],
              @Column("LastModified")		var lastmodified : Option[JavaDate]
              ) extends StringEntity {
  
  def autoUpdateFields = Set(
      "lastmodified",
      "created", 
      "initialfx", 
      "fixings", 
      "shortname", 
      "underlyinginfo", 
      "comment", 
      "settings", 
      "targetyield", 
      "currentcoupon", 
      "pricetype", 
      "defaultvol", 
      "cpnnotice",
      "bondname",
      "shortname",
      "description_jpn",
      "fixing_method",
      "model_output",
      "description_eng",
      "catchphrase",
      "characteristics",
      "chartsettings",
      "information"
  )
  
  def getFieldMap:Map[String, Any] = getObjectFieldMap(this)
  
  def isSameContent(b:Bond):Boolean = compareObjects(this, b, autoUpdateFields)
  
  def currency:Currency = Currencies(currencyid).orNull
		
  def calendar:Calendar = if (calendar_str == null) Calendars(currencyid).get
  						else Calendars(calendar_str.split(",").map(_.trim).toSet).getOrElse(Calendars(currencyid).get)
  
  protected def replaceFixing(p:String, fixings:Map[String, Any]):String = multipleReplace(p, fixings.map{case (k, v) => ("@" + k, v)})
  						
  protected def updateFixing(p:String):String = fixingInformation.update(p)
  						
  def fixingCoupon(fixings:Map[String, Any]):String = replaceFixing(if (coupon == null) "" else coupon, fixings + ("tbd" -> tbdValue.getOrElse("tbd")))
  
  def couponList:List[String] = stringList(updateFixing(coupon))
  
//  def couponList(fixings:Map[String, Any]):List[String] = stringList(updateFixing(coupon, fixings))
  
  def fixingRedemprice(fixings:Map[String, Any]):String = replaceFixing(if (redemprice == null) "" else redemprice, fixings + ("tbd" -> tbdValue.getOrElse("tbd")))
  
  def tbdParameter:Option[String] = (containsN(coupon, "tbd"), containsN(redemprice, "tbd"), containsN(call, "tbd")) match {
  	 case (true, false, false) => Some(coupon)
  	 case (false, true, false) => Some(redemprice)
  	 case (false, false, true) => Some(call)
  	 case _ => None
  }
  
  def containsN(s:String, t:String):Boolean = (s != null) && (s contains t)
  
  def containsTbd:Boolean = tbdParameter.isDefined
  
  def isPhysicalRedemption:Boolean = physicalredemption == 1
  
  def isJpyPayment:Boolean = jpypayment == 1
  
  def underlyingList:List[String] = stringList(underlying)
  
//  def bermudanList:List[Boolean] = booleanList(call, "berm")
  
//  def triggerList:List[List[String]] = call.jsonArray.map(_.parseStringList.map(_.orNull))
  
  def fixingMap:Map[String, Double] = fixings.parseJsonDoubleFields
  
  def settingMap:Map[String, String] = settings.parseJsonStringFields
  
  
  def tbdValue:Option[Double] = try{Some(settingMap("tbd").toDouble)} catch {case e:Throwable => None} 
  
  def descriptionjpnList:Map[String, String] = description_jpn.parseJsonStringFields
  
  def descriptionengList:Map[String, String] = description_eng.parseJsonStringFields
  
  def daycounter = Daycounters(daycount).getOrElse(new Actual365Fixed)
  
  def calendarAdjust = DayAdjustments.getOrElse(daycount_adj, BusinessDayConvention.ModifiedFollowing)
  
  def paymentAdjust = DayAdjustments.getOrElse(payment_adj, BusinessDayConvention.ModifiedFollowing)
  
  def maturityAdjust = DayAdjustments.getOrElse(daycount_adj, BusinessDayConvention.ModifiedFollowing)
  
  def period = (coupon_freq collect { case f => new qlPeriod(f, TimeUnit.Months)}).orNull

  def issueDate = Date(issuedate)
  
  def maturityDate = Date(maturity)
  
  def endDate:Date = terminationdate match {
    case Some(d) if maturity after d => Date(d)
    case _ => maturityDate
  }
  
  def isFixingInArrears = inarrears != Some(0)
  
  def couponNotice:Int = cpnnotice.getOrElse(5)
  
  def redemptionNotice:Int = redemnotice.getOrElse(couponNotice)
  
  def remainLife(valuedate:Date):Double = math.max(0.0, endDate.sub(valuedate).toDouble / 365.25)

  def schedule:Option[Schedule] = try {
    val schedule = Schedule(
        effectiveDate = issueDate,
		terminationDate = maturityDate,
		tenor = period,
		calendar = calendar,
		calendarConvention = calendarAdjust,
		paymentConvention = paymentAdjust,
		terminationDateConvention = maturityAdjust,
		rule = DateGeneration.Rule.Backward,
		fixingInArrears = isFixingInArrears,
		noticeDay = couponNotice,
		daycount = daycounter, 
		firstDate = None,
		nextToLastDate = None,
		addRedemption = true,
		maturityNotice = redemptionNotice
    )
    if (schedule == null) None else Some(schedule)
  }
  catch { case _:Throwable => None}
  
//  def bermudanList(nbLegs:Int = schedule.size):List[Boolean] = call.jsonNode match {
//  	case Some(b) if b.isArray && b.size == 1 => List.fill(nbLegs - 2)(b.head.parseString == Some("berm")) ++ List(false, false)
//	case Some(b) if b isArray => List.fill(nbLegs - 2 - b.size)(false) ++ b.map(_.parseString == Some("berm")).toList ++ List(false, false)
//	case _ => List.fill(nbLegs)(false)
//  }
//
//  def triggerList(nbLegs:Int = schedule.size):List[List[String]] = call.jsonNode match {
//    case Some(b) if b.isArray && b.size == 1 => 
////      List.fill(nbLegs - 2)(if (b.head isArray) b.head.map(_.parseDouble).toList else List.empty) ++ List.fill(2)(List.empty)
//      List.fill(nbLegs - 2)(if (b.head isArray) b.head.map(_.parseString.getOrElse("")).toList else List.empty) ++ List.fill(2)(List.empty)
//    case Some(b) if b isArray => 
////      List.fill(nbLegs - b.size - 2)(List.empty) ++ b.map(n => if (n isArray) n.map(optionalDouble).toList else List.empty) ++ List.fill(2)(List.empty)
//      List.fill(nbLegs - b.size - 2)(List.empty) ++ b.map(n => if (n isArray) n.map(_.parseString.getOrElse("")).toList else List.empty) ++ List.fill(2)(List.empty)
//    case _ => List.fill(nbLegs)(List.empty)
//  }
  
  private def optionalDouble(n:JsonNode):Option[Double] = 
    if (n == null || n.isNull) None
    else Some(n.parseDouble.getOrElse(Double.NaN))
    
  def isMatured(vd:Date):Boolean = (vd ge endDate) 
  
  def getInitialFixings:Map[String, Double] = 
    if (!fixingMap.isEmpty) fixingMap
    else if (isBeforeFixing) DB.getLatestPrices(underlyingList.toSet)
    else Map.empty
  
  def isBeforeFixing = (fixingdate, DB.latestParamDate) match {
    case (Some(f), Some(p)) => Date(f) gt p
    case _ => false
  }


  def settingsJson:JsonNode = settings.jsonNode.getOrElse((new ObjectMapper).createObjectNode)
  
  def modelOutputJson:Option[JsonNode] = try {Some(model_output.jsonNode.get)} catch {case e:Throwable => None}
  
  override def toString():String = "%-5s %-15s %-25s %-10s %-15s %-15s".format(id, issuedate.toString, maturity.toString, coupon, initialfx.toString, created.toString)

  @Transient
  lazy val fixingInformationObj = {
    def getDblSetting(key:String):Option[Double] = settingMap.get(key).flatMap{case v => try {Some(v.toDouble)} catch {case e:Throwable => None}}
    FixingInformation(getDblSetting("tbd"), getDblSetting("tbd_min"), getDblSetting("tbd_max"), getInitialFixings)
  }
  
  implicit def fixingInformation = fixingInformationObj
  
  def accrualPrice(vd:Date):Double = 
    if (isMatured(vd)) 0.0 
    else if (issueDate ge vd) issueprice.getOrElse(1.0)
    else try {
      val r = redemprice.toDouble
      val i = issueprice.get / 100.0
      val comp = math.pow(r / i, 1.0 / maturityDate.sub(issueDate).toDouble)
      i * math.pow(comp, vd.sub(issueDate).toDouble)
    } catch {
      case e:Throwable => issueprice match {
        case Some(p) => p / 100.0
        case None => 1.0}
      }
  
  def this() = this(
		id = null,
		ref_number = 0,
		filing = null,
		sellstart = null,
		sellend = null,
		issuedate = null,
		maturity = null,
		terminationdate = None,
		ismatured = 0,
		nominal = None,
		denomination = None,
		underlying = null,
		underlyinginfo = null,
		coupon = null,
		coupon_freq = None,
		coupon_peryr = -9999,
		daycount = null,
		daycount_adj = null,
		calendar_str = null, 
		payment_adj = null,
		inarrears = Some(0),
		cpnnotice = Some(0),
		issueprice = None,
		redemprice = null,
		redemnotice = Some(0),
		call = null,
		bondtype = null,
		initialfx = 0.0,
		fixingdate = None,
		fixings = null,
		isin = null,
		ticker = null,
		leadmanager = null,
		bondname = null,
		shortname = null,
		edinetname = null,
		description_jpn = null,
		description_eng = null, 
		currencyid = null,
		productid = null,
		issuerid = null,
		risktags = null,
		settings = null,
		pricetype = null,
		firstcoupon = None,
		currentcoupon = None,
		targetyield = None,
		defaultvol = -999.99,
		comment = null,
		jpypayment = 0,
		physicalredemption = 0,
		minimum_purchase = None,
		fixing_method = null,
    model_output = null,
    pricingid = null,
    catchphrase = null,
    characteristics = null,
    chartsettings = null,
    information = null,
		created = None,
		lastmodified  = None)
  
  
}

