package net.squantlib.database.schemadefinitions

import java.sql.Timestamp
import java.util.{Date => JavaDate}

import scala.language.postfixOps
import net.squantlib.util._
import net.squantlib.util.JsonUtils._
import net.squantlib.util.DisplayUtils._
import net.squantlib.schedule.{Schedule, ScheduledPayoffs}
import net.squantlib.util.initializer._
import net.squantlib.database.DB
import net.squantlib.util.RoundingInfo
import net.squantlib.util.ql.time.{BusinessDayConvention, DateGeneration, TimeUnit, Period => qlPeriod}
import net.squantlib.util.ql.daycounters._
import net.squantlib.util.ql.currencies.Currency
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.databind.ObjectMapper
import org.squeryl.annotations.Transient
import org.squeryl.annotations.Column
import org.squeryl.KeyedEntity

import scala.collection.JavaConverters._

class Bond(
  @Column("id")         override var id: String,
  @Column("ref_number")     var ref_number: Int,
  @Column("filed_on")       var filing: JavaDate,
  @Column("start_selling_on")      var sellstart: JavaDate,
  @Column("end_selling_on")      var sellend: JavaDate,
  @Column("issue_on")      var issuedate: JavaDate,
  @Column("coupon_start_on")      var coupon_start: JavaDate,
  @Column("settlement_on")     var settlement: JavaDate,
  @Column("redemption_on")     var maturity: JavaDate,
  @Column("terminated_on")  var terminationdate: Option[JavaDate],
  @Column("past_redemption")      var ismatured: Int,
  @Column("nominal_amount")      var nominal: Option[BigDecimal],
  @Column("denomination")   var denomination: Option[BigDecimal],
  @Column("underlying")     var underlying: String,
  @Column("underlyinginfo")   var underlyinginfo: String,
  @Column("coupon_rate")       var coupon: String,
  @Column("coupon_freq")    var coupon_freq: Option[Int],
  @Column("coupons_per_year")   var coupon_peryr: Int,
  @Column("daycount")     var daycount: String,
  @Column("daycount_adj")   var daycount_adj: String,
  @Column("payment_adj")    var payment_adj: String,
  @Column("calendar")     var calendar_str: String,
  @Column("paid_in_arrears")      var inarrears: Option[Int],
  @Column("coupon_notice")      var cpnnotice: Option[Int],
  @Column("issue_price")     var issueprice: Option[BigDecimal],
  @Column("redemption_price")     var redemprice: String,
  @Column("redem_notice")    var redemnotice: Option[Int],
  @Column("call_info")       var call: String,
  @Column("call_notice")       var callnotice: Int,
  @Column("category")       var bondtype: String,
  @Column("initial_forex_rate")      var initialfx: BigDecimal,
  @Column("fixing_on")     var fixingdate: Option[JavaDate],
  @Column("fixings")      var fixings: String,
  // @Column("isin")       var isin: String,
  // @Column("ticker")       var ticker: String,
  // @Column("lead_manager")    var leadmanager: String,
  @Column("name")     var bondname: String,
  @Column("short_name")      var shortname: String,
  @Column("edinet_name")     var edinetname: String,
  // @Column("description")  var description_jpn: String,
  // @Column("description_eng")  var description_eng: String,
  @Column("currency_id")     var currencyid: String,
  @Column("payment_currency_id")      var paymentCurrencyId: String,
  @Column("product_id")      var productid: String,
  @Column("issuer_id")     var issuerid: String,
  // @Column("risk_tags")       var risktags: String,
  @Column("settings")       var settings: String,
  @Column("firstcoupon_rate")    var firstcoupon: Option[Double],
  // @Column("currentcoupon_rate")    var currentcoupon:Option[Double],
  @Column("targetyield_rate")    var targetyield: Option[Double],
  // @Column("price_type")      var pricetype: String,
  // @Column("default_volatility")    var defaultvol: Double,
  // @Column("comment")      var comment: String,
  @Column("paid_in_jpy")    var jpypayment: Int,
  @Column("physical_redemption") var physicalredemption: Int,
  @Column("minimum_purchase")   var minimum_purchase: Option[BigDecimal],
  // @Column("fixing_method")      var fixing_method: String,
  @Column("model_output")      var model_output: String,
  @Column("pricing_id")      var pricingid: String,
  // @Column("catchphrase")      var catchphrase: String,
  // @Column("characteristics")  var characteristics: String,
  // @Column("chart_setting")    var chartsettings: String,
  // @Column("information")      var information: String,
  @Column("calculation_agent_id")      var calculation_agent_id: String,
  // @Column("initial_price")      var initial_price: Option[Double],
  // @Column("distributor_count")      var distributor_count: Int,
  // @Column("branch_count")      var branch_count: Int,
  @Column("fixing_calendar")      var fixing_calendar: String,
  @Column("fixing_page")      var fixing_page: String,
  @Column("subordinated")      var subordinated: Int,
  @Column("distribution_type")      var distribution_type: String,
  @Column("nominal_fixed")      var nominal_fixed: Int,
  @Column("bond_status")      var bond_status: String,
  @Column("collateral_info")      var collateral: String,
  // @Column("memo")      var memo: String,
  @Column("bond_note")      var bondnote: String,
  @Column("registration_on")      var registration_date: JavaDate,
  @Column("created_at")      override var created: Timestamp,
  @Column("updated_at")   override var lastmodified : Timestamp
) extends StringEntity {
  
  def autoUpdateFields = Set(
    "lastmodified",
    "created",
    "initialfx",
    "fixings",
    "shortname",
    "underlyinginfo",
    // "comment",
    "settings",
    "targetyield",
    // "currentcoupon",
    "pricetype",
    // "defaultvol",
    "cpnnotice",
    "bondname",
    "shortname",
    // "description_jpn",
    // "fixing_method",
    "model_output",
    // "description_eng",
    // "catchphrase",
    // "characteristics",
    // "chartsettings",
    // "information",
    // "initial_price",
    // "branch_count",
    // "distributor_count"
  )
  
  def getFieldMap:Map[String, Any] = getObjectFieldMap(this)
  
  def isSameContent(b:Bond):Boolean = compareObjects(this, b, autoUpdateFields)
  
  def currency:Currency = Currencies(currencyid).orNull
		
  def paymentCalendarSet:Set[String] = calendar_str.jsonArray.map(j => j.asText).toSet
    
  def paymentCalendar:DbCalendar =
    if (calendar_str == null) Calendars(currencyid).get
//  	else Calendars(calendar_str.split(",").map(_.trim).toSet).getOrElse(Calendars(currencyid).get)
    else {
      Calendars(paymentCalendarSet).getOrElse(Calendars(currencyid).get)
    }
  
  def fixingCalendarSet:Set[String] = fixing_calendar.jsonArray.map(j => j.asText).toSet
    
  def fixingCalendar:DbCalendar =
    if (fixing_calendar == null) Calendars(currencyid).get
//    else Calendars(calendar_str.split(",").map(_.trim).toSet).getOrElse(Calendars(currencyid).get)
    else {
      Calendars(fixingCalendarSet).getOrElse(Calendars(currencyid).get)
    }
  
  protected def replaceFixing(p:String, fixings:Map[String, Any]):String = multipleReplace(p, fixings.map{case (k, v) => ("@" + k, v)})
  						
  // protected def updateFixing(p:String):String = fixingInformation.update(p)
  						
  def fixingCoupon(fixings:Map[String, Any]):String = replaceFixing(if (coupon == null) "" else coupon, fixings + ("tbd" -> tbdValue.getOrElse("tbd")))
  
  // def couponList:List[String] = stringList(updateFixing(coupon))
  
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

  def pastReferenceUnderlyingIds:Set[String] = {
    underlyingSet.filter{case underlyingId => 
      val matchPattern = ("\\$" + underlyingId + "_[0-9]+L").r.unanchored
      (coupon != null && matchPattern.findFirstIn(coupon).isDefined) || (redemprice != null && matchPattern.findFirstIn(redemprice).isDefined) || (call != null && matchPattern.findFirstIn(call).isDefined)
    }
  }

  def containsPastReference:Boolean = !pastReferenceUnderlyingIds.isEmpty
  
  def isPhysicalRedemption:Boolean = physicalredemption == 1
  
  def isJpyPayment:Boolean = jpypayment == 1

  def underlyingList:List[String] = stringList(underlying)

  def underlyingSet:Set[String] = underlyingList.toSet

  def fixingMap:UnderlyingFixing = UnderlyingFixing(fixingMapDecimal) //UnderlyingFixing(fixingMapDouble)(fixingInformation)

  def fixingMapDecimal:Map[String, BigDecimal] = fixings.parseJsonDoubleFields.map{case (k, v) => (k, BigDecimal(v))}

  def settingMap:Map[String, String] = settings.parseJsonStringFields

  def tbdValue:Option[Double] = try{Some(settingMap("tbd").toDouble)} catch {case e:Throwable => None}
  
  def isAutoId:Boolean = try {
    settingMap.get("auto_id").collect{case d => d.toInt == 1}.getOrElse(false)
  } catch { case _:Throwable => false}

//  def redemptionFixingOnCoupon:Boolean = try {
//    settingMap.get("redemption_fixing_on_coupon").collect{case v => v.toInt == 1}.getOrElse(true)
//  } catch { case _:Throwable => true}
//
//  def callFixingOnCoupon:Boolean = try {
//    settingMap.get("call_fixing_on_coupon").collect{case v => v.toInt == 1}.getOrElse(true)
//  } catch { case _:Throwable => true}

  // def descriptionjpnList:Map[String, String] = description_jpn.parseJsonStringFields

  // def descriptionengList:Map[String, String] = description_eng.parseJsonStringFields

  // def getUniqueIds:Map[String, String] = {
  //   settings.jsonNode match {
  //     case Some(s) => s.getOption("uniq_ids") match {
  //       case Some(ss) => ss.parseStringFields
  //       case _ => Map.empty
  //     }
  //     case _ => Map.empty
  //   }
  // }

  def updateRefNumber(refId:Int) = {
    id = s"${issuerid}-${refId}"
    ref_number = refId
  }

  def daycounter = Daycounters(daycount).getOrElse(new Actual365Fixed)
  
  def calendarAdjust = DayAdjustments.getOrElse(daycount_adj, BusinessDayConvention.ModifiedFollowing)
  
  def paymentAdjust = DayAdjustments.getOrElse(payment_adj, BusinessDayConvention.ModifiedFollowing)
  
  def maturityAdjust = {
    val maturityAdjust:String = settingMap.getOrElse("maturity_adj", payment_adj)
    DayAdjustments.getOrElse(maturityAdjust, BusinessDayConvention.ModifiedFollowing)
  }

  def period = (coupon_freq collect { case f => new qlPeriod(f, TimeUnit.Months)}).orNull

  def issueDate = Date(issuedate)

  def settlementDate = Date(settlement)

  def maturityDate = Date(maturity)

  def isPerpetual:Boolean = maturityDate.year >= 2500

  def fixingDate = fixingdate.collect{case d => Date(d)}

  def endDate:Date = terminationdate match {
    case Some(d) if maturity after d => Date(d)
    case _ => maturityDate
  }
  
  def isFixingInArrears = inarrears != Some(0)
  
  def couponNotice:Int = cpnnotice.getOrElse(5)

  def getJsonDateArray(s:String, fieldName:String):List[Option[Date]] = try {
    val arr = s.jsonNode.collect{case node => node.asScala.toList.map(n => n.parseString(fieldName).flatMap{case s => Date.getDate(s)})}.getOrElse(List.empty)
    if (arr.exists(_.isDefined)) arr
    else List.empty
  } catch {case e : Throwable => List.empty}

  def couponFixingDates:List[Option[Date]] = getJsonDateArray(coupon, "fixed_on")

  def couponPaymentDates:List[Option[Date]] = getJsonDateArray(coupon, "payment_on")

  def callFixingDates:List[Option[Date]] = getJsonDateArray(call, "fixed_on")

  def callValueDates:List[Option[Date]] = getJsonDateArray(call, "value_on")

  def redemptionNotice:Int = redemnotice.getOrElse(couponNotice)

  def callNotice:Int = callnotice

  def remainLife(valuedate:Date):Double = math.max(0.0, endDate.sub(valuedate).toDouble / 365.25)
  
  @Transient
  lazy val lastRollDate:Option[Date] = {
    val bondSettings = settingMap
    
    bondSettings.get("lastroll").flatMap{case d => Date.getDate(d)} match {
	    case Some(d) => Some(d)

	    case _ => bondSettings.get("paymentdate").flatMap{case v => try {Some(v.toInt)} catch {case e:Throwable => None}} match {
	      case Some(dd) if dd >= 1 && dd <= 31 => 
	        val baseDate = maturityDate
	        if (dd == baseDate.dayOfMonth) None
	        else if (dd < baseDate.dayOfMonth) Some(Date(baseDate.year, baseDate.month, dd))
          else {
            val prevm = baseDate.addMonths(-1)
            Some(Date(prevm.year, prevm.month, dd))
          }
            
	      case _ => None
	    }
    }
  }

  @Transient
  lazy val calculationStartDate:Date = Date(coupon_start)

  @Transient
  lazy val firstPaymentDateAfter:Option[Date] = {
    val bondSettings = settingMap
    bondSettings.get("first_payment_after") match {
      case Some(d) => Date.getDate(d)
      case _ => None
    }
  }

  def defaultPaymentRounding(ccy:String):RoundingInfo = {
    RoundingInfo.defaultRounding(ccy, paymentRounding.roundType)
  }

  @Transient
  lazy val paymentRounding:RoundingInfo = {
    val roundInfo = settingsJson.get("payment_rounding")

    if (roundInfo != null) {
      RoundingInfo(roundInfo)
    } else {
      RoundingInfo.defaultRounding(currencyid)
    }
  }

  @Transient
  lazy val localPaymentRounding:RoundingInfo = {
    val roundInfo = settingsJson.get("local_payment_rounding")

    if (roundInfo != null) {
      RoundingInfo(roundInfo)
    } else {
      RoundingInfo.defaultRounding(paymentCurrencyId, paymentRounding.roundType)
    }
  }

  def getCouponRateRounding:RoundingInfo = {
    settingsJson.get("coupon_rate_rounding") match {
      case roundInfo if roundInfo != null => RoundingInfo(roundInfo, 2)
      case _ => RoundingInfo(10, "rounded")
    }
  }

  def couponRateRounding:RoundingInfo = fixingInformation.couponRateRounding

  def getPerformanceRounding:RoundingInfo = {
    settingsJson.get("performance_rounding") match {
      case roundInfo if roundInfo != null => 
        roundInfo.parseInt("precision") match {
          case Some(i) => RoundingInfo(roundInfo, 0)
          case _ => RoundingInfo(40, "none")
        }
        
      case _ => RoundingInfo(20, "rounded")
    }
  }

  def performanceRounding:RoundingInfo = fixingInformation.performanceRounding

  // @Transient
  // lazy val couponRateRounding:RoundingInfo = {
  //   settingsJson.get("coupon_rate_rounding") match {
  //     case roundInfo if roundInfo != null => RoundingInfo(roundInfo, 2)
  //     case _ => RoundingInfo(10, "rounded")
  //   }
  // }

  // @Transient
  // lazy val performanceRounding:RoundingInfo = {
  //   settingsJson.get("performance_rounding") match {
  //     case roundInfo if roundInfo != null => RoundingInfo(roundInfo, 2)
  //     case _ => RoundingInfo(20, "rounded")
  //   }
  // }

  @Transient
  lazy val partialAssetRoundings:Map[String, RoundingInfo] = {
    fixingInformation.partialAssetRounding
  }
 
  @Transient
  lazy val assetTradedUnit:Map[String, Int] = {
    fixingInformation.assetTradedUnit
  }

  def isRollMonthEnd:Boolean = try {
    settingMap.get("month_end_roll").collect { case d => d.toInt == 1 }.getOrElse(false)
  } catch { case _:Throwable => false}

  def customFixingDayOfMonth:Option[Int] = {
    try {
      settingMap.get("fixingdate").collect{case d => d.toInt}
    } catch { case _:Throwable => None}
  }

  def isFixingOnCalculationEndDate:Boolean = try {
    settingMap.get("fixing_on_enddate").collect { case d => d.toInt == 1 }.getOrElse(false)
  } catch { case _:Throwable => false}

  def fixingPageInformation:List[Map[String, String]] = fixing_page.jsonArray.map(jj => ExtendedJson(jj).parseStringFields)

  def schedule:Option[Schedule] = {
    try {
      val schedule = Schedule(
        effectiveDate = calculationStartDate, // (if (settlementDate == null) issueDate else settlementDate),
        terminationDate = maturityDate,
        tenor = period,
        fixingCalendar = fixingCalendar,
        fixingAdjustmentCalendar = getFixingAdjustmentCalendar,
        fixingAdjustmentConvention = getFixingAdjustmentConvention,
        paymentCalendar = paymentCalendar,
        calendarConvention = calendarAdjust,
        paymentConvention = paymentAdjust,
        terminationDateConvention = maturityAdjust,
        rule = DateGeneration.Rule.Backward,
        fixingInArrears = isFixingInArrears,
        couponNotice = couponNotice,
        redemptionNotice = Some(redemptionNotice), //(if (redemptionFixingOnCoupon) None else Some(redemptionNotice)),
        callNotice = Some(callNotice), //(if (callFixingOnCoupon) None else Some(callNotice)),
        daycounter = daycounter,
        firstDate = firstPaymentDateAfter,
        nextToLastDate = lastRollDate,
        addRedemption = true,
        fixedDayOfMonth = customFixingDayOfMonth,
        fixingOnCalculationEndDate = isFixingOnCalculationEndDate,
        rollMonthEnd = isRollMonthEnd,
        couponFixingDates = couponFixingDates,
        couponPaymentDates = couponPaymentDates,
        callFixingDates = callFixingDates,
        callValueDates = callValueDates
      )
      if (schedule == null) None else Some(schedule)
    }
    catch { case _:Throwable => None}
  }

  private def optionalDouble(n:JsonNode):Option[Double] = {
    if (n == null || n.isNull) None
    else Some(n.parseDouble.getOrElse(Double.NaN))
  }
    
  def isMatured(vd:Date):Boolean = (vd ge endDate)

  def getInitialFixings:UnderlyingFixing = UnderlyingFixing(getInitialFixingsDecimal) // (fixingInformation)

  def getInitialFixingsDecimal:Map[String, BigDecimal] = {
    val fMap = fixingMapDecimal

    if (!fMap.isEmpty) fMap
    else if (isBeforeFixing) DB.getLatestPrices(underlyingList.toSet).map{case (k, v) => (k, BigDecimal(v))}
    else Map.empty
  }
  
  def isBeforeFixing = (fixingdate, DB.latestParamDate) match {
    case (Some(f), Some(p)) => Date(f) gt p
    case _ => false
  }

  def getInitialFixingPrices:UnderlyingFixing = getInitialFixingPrices(underlyingList.toSet)

  def getInitialFixingPrices(ids:Set[String]):UnderlyingFixing = fixingDate match {
    case Some(d) =>
      val customFixings: Map[String, Double] = try {
        settingsJson.get("initial_fixing") match {
          case null => Map.empty
          case ns =>
            val fixings = ns.fieldNames.asScala.map(n => (n, ns.get(n).asDouble)).toMap
            fixings ++ fixings.map { case (k, v) => if (k.size == 6 && v > 0.0000001) Some((k.takeRight(3) + k.take(3), 1.0 / v)) else None }.flatMap(s => s).toMap
        }
      } catch {case _: Exception => Map.empty}

      val customFixingPrices = customFixings.filter{case (k, v) => ids.contains(k)}.map{case (k, v) => (k, BigDecimal(v))}

      val missingFixings:Map[String, Double] = {
        if ((ids -- customFixings.keySet).size > 0) getFixingPriceFromDb(ids, List(d), true).headOption.getOrElse(Map.empty)
        else Map.empty
      }

      (customFixingPrices.isEmpty, missingFixings.isEmpty) match {
        case (true, true) => UnderlyingFixing.empty
        case (false, true) => UnderlyingFixing(customFixingPrices)
        case (true, false) => UnderlyingFixing(missingFixings)(fixingInformation.getInitialFixingInformation)
        case (false, false) => UnderlyingFixing(UnderlyingFixing(missingFixings)(fixingInformation.getInitialFixingInformation).getDecimalValue ++ customFixingPrices)
      }

    case _ => UnderlyingFixing.empty
  }

  def getLegFixingPrices:Option[List[UnderlyingFixing]] = getLegFixingPrices(fixingInformation)

  def getLegFixingPrices(fixingInfo:FixingInformation):Option[List[UnderlyingFixing]] = getLegFixingPrices(underlyingSet, fixingInfo)

  def getLegFixingPrices(underlyingIds:Set[String], fixingInfo: FixingInformation):Option[List[UnderlyingFixing]] = {
    schedule.collect {case sche =>
      val fixingMap:Map[Date, UnderlyingFixing] = ScheduledPayoffs.getFixings(
        underlyingSet, 
        sche.eventDates.toSet,
        fixingInfo,
        false
      )

      sche.eventDates.map(d => fixingMap.getOrElse(d, UnderlyingFixing.empty))
    }
  }
  
  @Transient
  lazy val fixingAdjustmentCountryIds:Set[String] = {
    settingsJson.get("fixing_readjust_calendar") match {
      case null => Set.empty
      case ns => ns.parseStringList.flatMap{case s => s}.toSet //jsonArray.map(j => j.asText).toSet
    }
  }

  def getFixingAdjustmentCalendar:Option[DbCalendar] = {
    if (fixingAdjustmentCountryIds.isEmpty) None
    else {
      Calendars(fixingAdjustmentCountryIds)
    }
  }
  
  def getFixingAdjustmentConvention:BusinessDayConvention = {
    settingMap.get("fixing_readjust_convention") match {
      case Some(c) => DayAdjustments.getOrElse(c, BusinessDayConvention.Following)
      case _ => BusinessDayConvention.Following
    }
  }
  
  def getFixingPrices(dates:List[Date]):List[UnderlyingFixing] = getFixingPrices(underlyingList.toSet, dates)

  def getFixingPrices(ids:Set[String], dates:List[Date]):List[UnderlyingFixing] = {
    DB.getFixings(ids, dates, fixingInformation, false).map(ps => UnderlyingFixing(ps)(fixingInformation))
  }

  def getFixingPriceFromDb(ids:Set[String], dates:List[Date], isInitialFixing:Boolean) = DB.getFixings(ids, dates, fixingInformation, isInitialFixing)

  def getSingleFixingPriceFromDb(id:String, date:Date, isInitialFixing:Boolean):Option[Double] = getFixingPriceFromDb(Set(id), List(date), false).head.get(id)

  def settingsJson:ObjectNode = settings.objectNode.getOrElse((new ObjectMapper).createObjectNode)

  def modelOutputJson:Option[JsonNode] = try {Some(model_output.jsonNode.get)} catch {case e:Throwable => None}

  def modelOutputObject:ObjectNode = try {
    if (model_output != null && !model_output.isEmpty) model_output.objectNode.getOrElse(JsonUtils.newObjectNode)
    else JsonUtils.newObjectNode
  } catch {case _:Throwable => JsonUtils.newObjectNode}


  def modelOutputMap:Map[String, List[Any]] = modelOutputJson.collect{case j => j.parseAllFieldMap}.getOrElse(Map.empty)

  def isPublic:Boolean = (bond_status == "PUBLIC")

  override def toString():String = "%-5s %-15s %-25s %-10s %-15s %-15s".format(id, issuedate.toString, maturity.toString, coupon, initialfx.toString, created.toString)

  @Transient
  lazy val fixingInformationObj = {

    val currentSetting = settingMap

    def getDecimalSetting(key:String):Option[BigDecimal] = currentSetting.get(key)
      .flatMap{case v =>
        try {Some(BigDecimal(v))}
        catch {case e:Throwable => None}
      }

    val info = FixingInformation(
      currencyId = currencyid,
      paymentCurrencyId = paymentCurrencyId,
      tbd = getDecimalSetting("tbd"),
      minRange = getDecimalSetting("tbd_min"),
      maxRange = getDecimalSetting("tbd_max"),
      fixingPageInformation = fixingPageInformation,
      paymentCalendar = paymentCalendar,
      couponRateRounding = getCouponRateRounding,
      performanceRounding = getPerformanceRounding
    )

    info.setInitialFixingDecimal(getInitialFixingsDecimal)

    if (containsPastReference) {
      getLegFixingPrices(info) match {
        case None => 
        case Some(fixings) => info.setLegFixings(fixings)
      }
    }

    info
  }
  
  implicit def fixingInformation = fixingInformationObj

  def accrualPrice(vd:Date):Double = {
    if (isMatured(vd)) 0.0
    else if (issueDate ge vd) issueprice.collect { case p => p.toDouble / 100.0 }.getOrElse(1.0)
    else try {
      val r = redemprice.toDouble
      val i = issueprice.get.toDouble / 100.0
      val comp = math.pow(r / i, 1.0 / maturityDate.sub(issueDate).toDouble)
      i * math.pow(comp, vd.sub(issueDate).toDouble)
    } catch {
      case e: Throwable => issueprice match {
        case Some(p) => p.toDouble / 100.0
        case None => 1.0
      }
    }
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
    currentjson.set(name, newnode)
    setter(currentjson.toJsonString)
  }
  
  protected def setJsonObject(name:String, newValue:String)(implicit getter:() => String, setter:String => Unit):Unit = {
    val currentjson = getJsonObject(getter)
    currentjson.put(name, newValue)
    setter(currentjson.toJsonString)
  }
  
  protected def setJsonObject(nodes:Map[String, JsonNode])(implicit getter:() => String, setter:String => Unit):Unit = {
    val currentjson = getJsonObject(getter)
    nodes.foreach{case (k, v) => currentjson.set(k, v)}
    setter(currentjson.toJsonString)
  }

  def reassignId(newId:String) = {
    id = newId
  }

  def defaultVolatility:Double = 0.1
    
  def getSetting:ObjectNode = getJsonObject(() => this.settings)
  def setSetting(name:String, newValue:String):Unit = setJsonObject(name, newValue)(() => this.settings, (s:String) => settings = s)
  def setSetting(name:String, newNode:JsonNode):Unit = setJsonObject(name, newNode)(() => this.settings, (s:String) => settings = s)
  
  def getModelOutput(id:String):ObjectNode = getJsonObject(() => this.model_output)
  def setModelOutput(id:String, name:String, newValue:String):Unit = setJsonObject(name, newValue)(() => this.model_output, (s:String) => this.model_output = s)
  def setModelOutput(id:String, name:String, newNode:JsonNode):Unit = setJsonObject(name, newNode)(() => this.model_output, (s:String) => this.model_output = s)
  
  // def getInformation:ObjectNode = getJsonObject(() => this.information)
  // def setInformation(name:String, newValue:String):Unit = setJsonObject(name, newValue)(() => this.information, (s:String) => this.information = s)
  // def setInformation(name:String, newNode:JsonNode):Unit = setJsonObject(name, newNode)(() => this.information, (s:String) => this.information = s)
  // def setInformation(nodes:Map[String, JsonNode]):Unit = setJsonObject(nodes)(() => this.information, (s:String) => this.information = s)
  
  // def getCharacteristics:ObjectNode = getJsonObject(() => this.characteristics)
  // def setCharacteristics(name:String, newValue:String):Unit = setJsonObject(name, newValue)(() => this.characteristics, (s:String) => this.characteristics = s)
  // def setCharacteristics(name:String, newNode:JsonNode):Unit = setJsonObject(name, newNode)(() => this.characteristics, (s:String) => this.characteristics = s)
  
  // def getChartSettings:ObjectNode = getJsonObject(() => this.chartsettings)
  // def setChartSettings(name:String, newValue:String):Unit = setJsonObject(name, newValue)(() => this.chartsettings, (s:String) => this.chartsettings = s)
  // def setChartSettings(name:String, newNode:JsonNode):Unit = setJsonObject(name, newNode)(() => this.chartsettings, (s:String) => this.chartsettings = s)
  
  def this() = this(
    id = null,
    ref_number = 0,
    filing = null,
    sellstart = null,
    sellend = null,
    settlement = null,
    issuedate = null,
    coupon_start = null,
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
    callnotice = 0,
    bondtype = null,
    initialfx = 0.0,
    fixingdate = None,
    fixings = null,
    // isin = null,
    // ticker = null,
    // leadmanager = null,
    bondname = null,
    shortname = null,
    edinetname = null,
    // description_jpn = null,
    // description_eng = null,
    currencyid = null,
    paymentCurrencyId = null,
    productid = null,
    issuerid = null,
    // risktags = null,
    settings = null,
    // pricetype = null,
    firstcoupon = None,
    // currentcoupon = None,
    targetyield = None,
    // defaultvol = -999.99,
    // comment = null,
    jpypayment = 0,
    physicalredemption = 0,
    minimum_purchase = None,
    // fixing_method = null,
    model_output = null,
    pricingid = null,
    // catchphrase = null,
    // characteristics = null,
    // chartsettings = null,
    // information = null,
    calculation_agent_id = null,
    // initial_price = None,
    // distributor_count = 0,
    // branch_count = 0,
    fixing_calendar = null,
    fixing_page = null,
    subordinated = 0,
    distribution_type = null,
    nominal_fixed = 1,
    bond_status = null,
    collateral = null,
    // memo = null,
    bondnote = null,
    registration_date = null,
    created = null,
    lastmodified = null
  )
  
}

