package net.squantlib.schedule.payoff

import java.util.{Map => JavaMap}

import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.util._
import net.squantlib.database.DB
import net.squantlib.model.market.Market

import scala.Option.option2Iterable
import net.squantlib.schedule.{CalculationPeriod, FixingLeg, KnockInCondition}
import net.squantlib.schedule.baskettypes._
import com.fasterxml.jackson.databind.JsonNode

import scala.reflect.ClassTag
import scala.collection.JavaConverters._
import com.fasterxml.jackson.databind.ObjectMapper

trait Payoff extends FixingLeg {

  /*
   * List of reference variables.
   * Price fixings should be provided by map variable -> fixing value.
   */
  override val variables:Set[String]

  implicit val fixingInfo: FixingInformation // to be implemented

  val isPriceable:Boolean // checks priceablility of the payoff

  def dateShifted(d:Int):Payoff = this

  var isAbsolute:Boolean = false

  def setAbsolute = {
    isAbsolute = true
  }

  val physical:Boolean = false

  override def isSettlementFixed:Boolean = variables.isEmpty || !physical || !settlementFixings.isEmpty

  var nominal:Double = 1.0

  val currencyId:String = fixingInfo.currencyId

  val minPayoff:Double = 0.0

  val maxPayoff:Option[Double] = None

  var overrideFixedRate:Option[BigDecimal] = None

  def paymentCurrencyId:String = fixingInfo.paymentCurrencyId

  def paymentAssetId:String = fixingInfo.paymentCurrencyId

  def adjustmentInfo:Option[JsonNode] = None

  def paymentAmount(denomination:BigDecimal, dayCount:Double):Option[BigDecimal] = {
    if (isFixed && !price.isNaN && !price.isInfinity) {
      Some(price * dayCount * denomination)
    } else None
  }

  val keywords:Set[String] = jsonMapImpl.keySet

  def jsonMap:Map[String, Any] = inputString.jsonNode match {
    case Some(node) =>
      val fieldnames = node.fieldNames.asScala.filter(n => !keywords.contains(n)).toSet
      fieldnames.map(n => (n -> node.get(n))).toMap ++ jsonMapImpl
    case _ => jsonMapImpl
  }

  def jsonString:String = {
    val infoMap:JavaMap[String, Any] = jsonMap.asJava
    JsonUtils.jsonString(infoMap)
    //(new ObjectMapper).writeValueAsString(infoMap)
  }

  def isPaymentFixed:Boolean = isFixed && (!physical || isSettlementFixed)

  def jsonMapImpl:Map[String, Any]

  def fixedConditions:Map[String, Any] = Map.empty

  def fixedConditionsString:String = {
    JsonUtils.jsonString(fixedConditions.asJava)
  }

  /*
   * Price assuming all variables fixed at spot market.
   */

  def price(market:Market, pastPayments:List[Double]):Double = {
    priceFlat(UnderlyingFixing(market.getFixings(variables)), pastPayments)
  }

  final def price(market:Market, period:CalculationPeriod, pastPayments:List[Double]):Double = price(market, eventDates(period).size, pastPayments)

  final def price(market:Market, fixingCount:Int, pastPayments:List[Double]):Double = {
    val mktFixings = UnderlyingFixing(market.getFixings(variables))
    price(List.fill(fixingCount)(mktFixings), pastPayments)
  }

  /*
   * Returns price if there's no variable. Returns NaN in case of >0 variables.
   */

  final def price:Double = price(null)

  final def price(priceResult:PriceResult):Double = {
    if (isPaymentFixed) priceImpl(List.fill(2)(getFixings), List.empty, priceResult)
    else if (isPriceable) priceImpl(priceResult)
    else Double.NaN
  }

  /*
   * Price in case of multiple event dates and multiple variables.
   * Only refers to the last variable by default but can be overridden.
   */
  final def price(fixings:List[UnderlyingFixing], pastPayments:List[Double]):Double = price(fixings, pastPayments, null)

  final def price(fixings:List[UnderlyingFixing], pastPayments:List[Double], priceResult:PriceResult):Double = {
    if (isPaymentFixed) priceImpl(List.fill(2)(getFixings), pastPayments, priceResult)
    else if (isPriceable) priceImpl(fixings, pastPayments, priceResult)
    else Double.NaN
  }

  final def price(underlyingId:String, fixings:List[Double], pastPayments:List[Double]):Double = {
    price(fixings.map(f => UnderlyingFixing(Map(underlyingId -> f))), pastPayments)
  }

  final def price(underlyingId:String, fixings:List[Double], pastPayments:List[Double], priceResult:PriceResult):Double = {
    price(fixings.map(f => UnderlyingFixing(Map(underlyingId -> f))), pastPayments, priceResult)
  }

  final def price(fixings:List[Map[String, Double]], pastPayments:List[Double])(implicit dummyImplicit: DummyImplicit):Double = {
    price(fixings.map(fs => UnderlyingFixing(fs)), pastPayments)
  }

  final def price(fixings:List[Map[String, Double]], pastPayments:List[Double], priceResult:PriceResult)(implicit dummyImplicit: DummyImplicit):Double = {
    price(fixings.map(fs => UnderlyingFixing(fs)), pastPayments, priceResult)
  }

  def priceImpl(priceResult:PriceResult):Double

  // PAST PAYMENTS ARE REVERSED (recent first)
  def priceImpl(fixings:List[UnderlyingFixing], pastPayments:List[Double], priceResult:PriceResult):Double = {
    if (fixings.isEmpty) price
    else price(fixings, pastPayments)
  }

  def priceFlat(fixings:UnderlyingFixing, pastPayments:List[Double]):Double = price(List.fill(2)(fixings), pastPayments)

  final def priceWithInfo:PriceResult = {
    val priceResult = new PriceResult
    val p = price(priceResult)
    priceResult.setPrice(p)
    priceResult
  }

  final def priceWithInfo(fixings:List[UnderlyingFixing], pastPayments:List[Double]):PriceResult = {
    val priceResult = new PriceResult
    val p = price(fixings, pastPayments, priceResult)
    priceResult.setPrice(p)
    priceResult
  }

  /*
   * Event dates, usually used in case of >1 fixing dates.
   */
  def eventDates(d:CalculationPeriod):List[Date] = {
    if (physical) List(d.eventDate, d.paymentDate)
    else List(d.eventDate)
  }

  val eventDateFixingIndexFromRight:Int = if (physical) 2 else 1 //10

  def getEventDateFixing[T](fixings:List[T]):Option[T] = {
    fixings.takeRight(eventDateFixingIndexFromRight).headOption
  }

  def triggeredUnderlyings(fixing:UnderlyingFixing, trigger:UnderlyingFixing, triggerUp:Boolean):Set[String] = {
    trigger.getDecimal.filter{case (ul, v) =>
      if (triggerUp) fixing.getDecimal.get(ul).collect{case fv => fv >= v}.getOrElse(false)
      else fixing.getDecimal.get(ul).collect{case fv => fv <= v}.getOrElse(false)
    }.keySet
  }

  def isTriggered(fixing:UnderlyingFixing, trigger:UnderlyingFixing, triggerUp:Boolean):Boolean =
    !trigger.isEmpty && triggeredUnderlyings(fixing, trigger, triggerUp).size == trigger.size

  def terminationAmount(fixing:UnderlyingFixing, trigNominal:Double, forwardStrikes:Option[UnderlyingFixing]):Double = forwardStrikes match {
    case Some(fwd) => trigNominal * fwd.getDouble.map{case (k, v) => fixing.getDouble(k) / v}.min
    case _ => trigNominal
  }


  /*
   * Returns FixedPayoff if all variables fixing are available on given date.
   * Returns this object if any variables are missing.
   */

  def assignFixings(f:UnderlyingFixing, pastPayments:List[Double]):Unit = super.assignFixings(f)

  //def assignFixings(f:Double, pastPayments:List[Double]):Unit = super.assignFixings(f)

  def assignFixings(eventDate:Date, pastPayments:List[Double]):Unit =
    if (variables.size == 0) {}
    else {
      val fixings:UnderlyingFixing = UnderlyingFixing(variables.map(v => DB.getPriceOn(v, eventDate).collect{case (_, f) => (v, f)}).flatMap(x => x).toMap)
      assignFixings(fixings)
    }

  /*
   * Returns FixedPayoff if all variables fixing are available on given market.
   * Returns this object if any variables are missing.
   */
  def assignFixings(market:Market, pastPayments:List[Double]):Unit = assignFixings(UnderlyingFixing(market.getFixings(variables)))

  override def assignSettlementFixings(f:UnderlyingFixing):Unit = {
    if ((physical && f.isValidFor(variables)) || f.isEmpty) {
      settlementFixings = f
    }
  }

  def getRedemption:Option[Payoff] = {
    inputString.jsonNode("redemption") match {
      case Some(s) => Payoff(s.toJsonString)(fixingInfo)
      case _ => None
    }
  }

  def missingInputs:Map[String, Double => Payoff] = Map.empty

  def withMinMax(r:Double) = {
    maxPayoff match {
      case Some(v) => Math.min(Math.max(r, minPayoff), v)
      case None => Math.max(r, minPayoff)
    }
  }

  def withMinMax(r:BigDecimal):BigDecimal= {
    maxPayoff match {
      case Some(v) => r.max(minPayoff).min(v)
      case None => r.max(minPayoff)
    }
  }

  def description:String

  def inputString:String

  def toString:String
}

object Payoff {

  def apply(inputString:String)(implicit fixingInfo:FixingInformation):Option[Payoff] = {
    if (inputString == null || inputString.trim.isEmpty) Some(NullPayoff(""))
    else {
      val po = payoffType(inputString) match {
        case "fixed" => FixedPayoff(inputString)
        case "leps1d" => LEPS1dPayoff(inputString)
        case "putdi" => PutDIPayoff(inputString)
        case "putdiamerican" => PutDIAmericanPayoff(inputString)
        case "callui" => CallUIPayoff(inputString)
        case "forward" => ForwardPayoff(inputString)
        case "rangeforward" => RangeForwardPayoff(inputString)
        case "null" => NullPayoff(inputString)
        case "binary" => BinaryPayoff(inputString)
        case "rangeaccrual" => RangeAccrualPayoff(inputString)
        case "general" => GeneralPayoff(inputString)
        case _ => GeneralPayoff(inputString)
      }

      if (isAbsolute(inputString)) {
        po.isAbsolute = true
      }

      customNominal(inputString) match {
        case Some(n) => po.nominal = n
        case _ => {}
      }

      val variableList:List[String] = inputString.parseJsonStringList("variable").map(_.orNull)

      if (!variableList.isEmpty) {
        inputString.jsonNode.collect{
          case n => {
            val overrideUnderlyingFixings = Payoff.nodeToComputedMap(n, "fixings", variableList).getOptionalDecimal()(fixingInfo.getStrikeFixingInformation)
            println(overrideUnderlyingFixings)
            if (!overrideUnderlyingFixings.isEmpty) {
              po.assignOverrideFixings(UnderlyingFixing(overrideUnderlyingFixings))
            }
          }
        }
      }

      Some(po)
    }
  }

  def payoffType(formula:String):String = formula match {
    case f if f.parseDouble.isDefined => "fixed"
    case f => formula.parseJsonString("type").orNull
  }

  def isAbsolute(formula:String):Boolean = formula match {
    case f if f.parseDouble.isDefined => false
    case f => formula.parseJsonString("absolute") == Some("1")
  }

  def customNominal(formula:String):Option[Double] = formula match {
    case f if f.parseDouble.isDefined => None
    case f => formula.parseJsonDouble("nominal")
  }


  def simpleCashFlow(currencyId:String, paymentCurrencyId:String, amount:Double) = FixedPayoff(amount)(FixingInformation.empty(currencyId, paymentCurrencyId))

  def updateReplacements(inputString:String)(implicit fixingInfo:FixingInformation):String = {
    if (!inputString.trim.startsWith("{")) {return inputString}

    var result = inputString
    inputString.jsonNode match {
      case Some(node) =>
	      val fieldnames = node.fieldNames.asScala.filter(n => n.startsWith("^")).toSet
        fieldnames.foreach(n => {
          val currentvalue = node.get(n).textValue()
          val updatedvalue = fixingInfo.update(currentvalue)
          result = result.replace(n, FormulaParser.calculate(updatedvalue).getOrElse(Double.NaN).toString)
        })
	    case None => {}
	  }
    result
  }

  def nodeToComputedMap(
    node:JsonNode,
    s:String,
    variable:List[String]
  )(implicit fixingInfo:FixingInformation):Map[String, Double] = nodeToComputedMap(node, s, variable, Set.empty)

  def nodeToComputedMap(
    node:JsonNode,
    s:String,
    variable:List[String],
    ignoredInputs:Set[String]
  )(implicit fixingInfo:FixingInformation):Map[String, Double] = {
    node.getOption(s) match {
      case Some(n) =>
        JsonUtils.nodeToHashMap(n, variable)
          .filter{case (k, v) => v != null && v != "null" && !ignoredInputs.contains(v)}
          .map{case (k, v) => (k, fixingInfo.updateCompute(v))}
          .collect { case (k, v) => (k, v.getOrElse(Double.NaN)) }
          .toMap
      case _ => Map.empty
    }
  }

  def initializeCouponReset(node:JsonNode)(implicit strikeFixingInfo:FixingInformation):KnockInCondition = {
    val strikeFormula:Map[String, String] = node.get("reset_strike").parseStringFields
    val computedStrikeFormula:Map[String, Double] = strikeFormula.map{case (uid, s) => (uid, strikeFixingInfo.updateCompute(s, uid).getOrElse(Double.NaN))}
    val strikeValues:UnderlyingFixing = UnderlyingFixing(computedStrikeFormula)(strikeFixingInfo)

    if (strikeValues.isEmpty) KnockInCondition.empty
    else {
      (node.get("reset_refstart").parseDate, node.get("reset_refend").parseDate) match {
        case (Some(dStart), Some(dEnd)) =>
          KnockInCondition(
            trigger = strikeValues,
            refStart = dStart,
            refEnd = dEnd,
            finalTrigger = UnderlyingFixing.empty,
            closeOnly = node.get("reset_reftype").parseString.collect{case i => i == "closing"}.getOrElse(true),
            triggerDown = node.get("reset_down").parseInt.collect{case i => i != 0}.getOrElse(true),
            triggerOnEqual = node.get("reset_on_equal").parseInt.collect{case i => i == 1}.getOrElse(true),
            basketType = WorstOf
          )
        case _ => KnockInCondition.empty
      }
    }
  }

}
