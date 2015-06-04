package net.squantlib.database.schemadefinitions

import java.sql.Timestamp
import java.util.{Date => JavaDate}
import scala.collection.JavaConversions._
import scala.language.postfixOps
import net.squantlib.util.JsonUtils
import net.squantlib.util.JsonUtils._
import net.squantlib.schedule.Schedule
import net.squantlib.util.initializer._
import net.squantlib.database.DB
import net.squantlib.util.{FixingInformation, Date}
import org.jquantlib.time.{Period => qlPeriod, DateGeneration, Calendar, BusinessDayConvention, TimeUnit}
import org.jquantlib.daycounters._
import org.jquantlib.currencies.Currency
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node.ObjectNode
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
              @Column("CalculationAgentID")      var calculation_agent_id: String,
              @Column("InitialPrice")      var initial_price: Option[Double],
              @Column("DistributorCount")      var distributor_count: Int,
              @Column("BranchCount")      var branch_count: Int,
              @Column("FixingCalendar")      var fixing_calendar: String,
              @Column("FixingPage")      var fixing_page: String,
              @Column("Subordinated")      var subordinated: Int,
              @Column("Created")			override var created: Timestamp,
              @Column("LastModified")		override var lastmodified : Timestamp
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
      "information",
      "initial_price",
      "branch_count",
      "distributor_count"
  )
  
  def getFieldMap:Map[String, Any] = getObjectFieldMap(this)
  
  def isSameContent(b:Bond):Boolean = compareObjects(this, b, autoUpdateFields)
  
  def currency:Currency = Currencies(currencyid).orNull
		
  def paymentCalendarSet:Set[String] = calendar_str.jsonArray.map(j => j.asText).toSet
    
  def paymentCalendar:Calendar = 
    if (calendar_str == null) Calendars(currencyid).get
//  	else Calendars(calendar_str.split(",").map(_.trim).toSet).getOrElse(Calendars(currencyid).get)
    else {
      Calendars(paymentCalendarSet).getOrElse(Calendars(currencyid).get)
    }
  
  def fixingCalendarSet:Set[String] = fixing_calendar.jsonArray.map(j => j.asText).toSet
    
  def fixingCalendar:Calendar = 
    if (fixing_calendar == null) Calendars(currencyid).get
//    else Calendars(calendar_str.split(",").map(_.trim).toSet).getOrElse(Calendars(currencyid).get)
    else {
      Calendars(fixingCalendarSet).getOrElse(Calendars(currencyid).get)
    }
  
  protected def replaceFixing(p:String, fixings:Map[String, Any]):String = multipleReplace(p, fixings.map{case (k, v) => ("@" + k, v)})
  						
  protected def updateFixing(p:String):String = fixingInformation.update(p)
  						
  def fixingCoupon(fixings:Map[String, Any]):String = replaceFixing(if (coupon == null) "" else coupon, fixings + ("tbd" -> tbdValue.getOrElse("tbd")))
  
  def couponList:List[String] = stringList(updateFixing(coupon))
  
  def fixingRedemprice(fixings:Map[String, Any]):String = replaceFixing(if (redemprice == null) "" else redemprice, fixings + ("tbd" -> tbdValue.getOrElse("tbd")))
  
  def tbdParameter:Option[String] = List(
    if (coupon != null && coupon.contains("tbd")) Some(coupon) else None,
    if (redemprice != null && redemprice.contains("tbd")) Some(redemprice) else None,
    if (call != null && call.contains("tbd")) Some(call) else None
  ).flatMap(s => s) match {
    case ls if ls.isEmpty => None
    case ls => Some(ls.mkString("; "))
  }
  
  def containsTbd:Boolean = tbdParameter.isDefined
  
  def isPhysicalRedemption:Boolean = physicalredemption == 1
  
  def isJpyPayment:Boolean = jpypayment == 1
  
  def underlyingList:List[String] = stringList(underlying)
  
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
  
  def fixingDate = fixingdate.collect{case d => Date(d)}
  
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
        fixingCalendar = fixingCalendar,
        paymentCalendar = paymentCalendar,
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

  def modelOutputMap:Map[String, List[Any]] = modelOutputJson.collect{case j => j.parseAllFieldMap}.getOrElse(Map.empty)
  
  override def toString():String = "%-5s %-15s %-25s %-10s %-15s %-15s".format(id, issuedate.toString, maturity.toString, coupon, initialfx.toString, created.toString)

  @Transient
  lazy val fixingInformationObj = {
    def getDblSetting(key:String):Option[Double] = settingMap.get(key).flatMap{case v => try {Some(v.toDouble)} catch {case e:Throwable => None}}
    FixingInformation(getDblSetting("tbd"), getDblSetting("tbd_min"), getDblSetting("tbd_max"), getInitialFixings)
  }
  
  implicit def fixingInformation = fixingInformationObj
  
  def accrualPrice(vd:Date):Double = 
    if (isMatured(vd)) 0.0 
    else if (issueDate ge vd) issueprice.collect{case p => p / 100.0}.getOrElse(1.0)
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
    
  /*
   * Accessor for Json parameters
   */
    
  protected def getJsonObject(implicit getter: () => String):ObjectNode = {
    val s = getter()
    if (s != null && !s.isEmpty) s.objectNode.getOrElse(JsonUtils.newObjectNode)
    else JsonUtils.newObjectNode
  }
  
  protected def removeJsonObject(name:String)(implicit getter:() => String, setter:String => Unit):Unit = {
    val currentjson = getJsonObject(getter)
    currentjson.remove(name)
    setter(currentjson.toJsonString)
  }
  
  protected def setJsonObject(name:String, newnode:JsonNode)(implicit getter:() => String, setter:String => Unit):Unit = {
    val currentjson = getJsonObject(getter)
    currentjson.put(name, newnode)
    setter(currentjson.toJsonString)
  }
  
  protected def setJsonObject(name:String, newValue:String)(implicit getter:() => String, setter:String => Unit):Unit = {
    val currentjson = getJsonObject(getter)
    currentjson.put(name, newValue)
    setter(currentjson.toJsonString)
  }
  
  protected def setJsonObject(nodes:Map[String, JsonNode])(implicit getter:() => String, setter:String => Unit):Unit = {
    val currentjson = getJsonObject(getter)
    nodes.foreach{case (k, v) => currentjson.put(k, v)}
    setter(currentjson.toJsonString)
  } 
    
  def getSetting:ObjectNode = getJsonObject(() => this.settings)
  def setSetting(name:String, newValue:String):Unit = setJsonObject(name, newValue)(() => this.settings, (s:String) => settings = s)
  def setSetting(name:String, newNode:JsonNode):Unit = setJsonObject(name, newNode)(() => this.settings, (s:String) => settings = s)
  
  def getModelOutput(id:String):ObjectNode = getJsonObject(() => this.model_output)
  def setModelOutput(id:String, name:String, newValue:String):Unit = setJsonObject(name, newValue)(() => this.model_output, (s:String) => this.model_output = s)
  def setModelOutput(id:String, name:String, newNode:JsonNode):Unit = setJsonObject(name, newNode)(() => this.model_output, (s:String) => this.model_output = s)
  
  def getInformation:ObjectNode = getJsonObject(() => this.information)
  def setInformation(name:String, newValue:String):Unit = setJsonObject(name, newValue)(() => this.information, (s:String) => this.information = s)
  def setInformation(name:String, newNode:JsonNode):Unit = setJsonObject(name, newNode)(() => this.information, (s:String) => this.information = s)
  def setInformation(nodes:Map[String, JsonNode]):Unit = setJsonObject(nodes)(() => this.information, (s:String) => this.information = s)
  
  def getCharacteristics:ObjectNode = getJsonObject(() => this.characteristics)
  def setCharacteristics(name:String, newValue:String):Unit = setJsonObject(name, newValue)(() => this.characteristics, (s:String) => this.characteristics = s)
  def setCharacteristics(name:String, newNode:JsonNode):Unit = setJsonObject(name, newNode)(() => this.characteristics, (s:String) => this.characteristics = s)
  
  def getChartSettings:ObjectNode = getJsonObject(() => this.chartsettings)
  def setChartSettings(name:String, newValue:String):Unit = setJsonObject(name, newValue)(() => this.chartsettings, (s:String) => this.chartsettings = s)
  def setChartSettings(name:String, newNode:JsonNode):Unit = setJsonObject(name, newNode)(() => this.chartsettings, (s:String) => this.chartsettings = s)
  
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
		calculation_agent_id = null,
		initial_price = None,
		distributor_count = 0,
		branch_count = 0,
		fixing_calendar = null,
		fixing_page = null,
		subordinated = 0,
		created = null,
		lastmodified  = null)
  
}

