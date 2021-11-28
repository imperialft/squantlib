package net.squantlib.schedule.payoff

import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.database.DB
import net.squantlib.util.{Date, FixingInformation, UnderlyingFixing}
import net.squantlib.schedule.CalculationPeriod
import net.squantlib.schedule.baskettypes._
import net.squantlib.model.market.Market
import scala.collection.JavaConverters._


/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 *  {type:"putdiamerican", variable:[String], trigger:[Double], strike:[Double], refstart:Date, refend:Date, description:String}, 
 * No strike is considered as no low boundary
 */
case class PutDIAmericanPayoff(
  triggers: UnderlyingFixing,
  strikes: UnderlyingFixing,
  finalTriggers: UnderlyingFixing,
  finalTriggerBasketType: BasketType,
  refstart:Date,
  refend:Date,
  refDates: Set[Date],
  var knockedIn:Boolean,
  override val physical:Boolean,
  forward:Boolean,
  closeOnly:Boolean,
  reverse: Boolean,
  leverage: Double,
  override val minPayoff: Double,
  override val maxPayoff: Option[Double],
  knockInOnEqual: Boolean,
  amount: Double = 1.0,
  description:String = null,
  inputString:String = null
)(implicit val fixingInfo:FixingInformation) extends Payoff {

  override val variables = triggers.keySet ++ strikes.keySet ++ finalTriggers.keySet

  val strikeVariables = strikes.keySet
  val triggerVariables = triggers.keySet
  val strikeOrFinalTriggerVariables = finalTriggers.keySet

  nominal = amount
  
  val strikeOrFinalTriggers:UnderlyingFixing = {
    val mktparam = (strikes.keySet ++ finalTriggers.keySet).map(v =>
      (strikes.getDecimalValue.get(v), finalTriggers.getDecimalValue.get(v)) match {
        case (Some(s), Some(t)) => (v, s.min(t))
        case (None, Some(t)) => (v, t)
        case (Some(s), None) => (v, s)
        case _ => (v, BigDecimal.valueOf(999999999999.0))
      }
    ).toMap
    UnderlyingFixing(mktparam)
  }

  override val isPriceable:Boolean = {
    !triggers.isEmpty &&
    triggers.isAllValid &&
    strikes.isPositive &&
    refstart != null &&
    refend != null &&
    (refstart le refend) &&
    finalTriggers.isAllValid
  }

  var mcPeriod6m = 30
  var mcPeriod1y = 90
  var mcPeriodbefore = 180
  
  override def eventDates(period:CalculationPeriod):List[Date] = {
    if (!isPriceable) {return List(period.endDate)}

    val mcStart = refstart.serialNumber
    val mcEnd = Math.min(refend.serialNumber, period.eventDate.serialNumber)
    val basemod = mcEnd % mcPeriod6m

    val dates:List[Date] = {
      if (mcStart <= mcEnd) {
        (
          for (i <- (mcStart to mcEnd)
               if (i >= mcEnd - 180 && i % mcPeriod6m == basemod)
                 || (i >= mcEnd - 360 && i % mcPeriod1y == basemod)
                 || (i % mcPeriodbefore == basemod)
          ) yield Date(i)
          ) (collection.breakOut)
      } else List.empty //List(Date(mcEnd))
    }

    if (physical) {
      dates.headOption match {
        case Some(d) if d == refstart => dates :+ period.paymentDate
        case Some(_) => (refstart :: dates) :+ period.paymentDate
        case None => List.empty
      }
    } else {
      dates.headOption match {
        case Some(d) if d == refstart => dates
        case Some(_) => refstart :: dates
        case None => List.empty
      }
    }

//    if (physical) {
//      if (dates.head == refstart) dates :+ period.paymentDate else (refstart :: dates) :+ period.paymentDate
//    } else {
//      if (dates.head == refstart) dates else refstart :: dates
//    }
  }

  def isKnockedInPrice(p:BigDecimal, trig:BigDecimal):Boolean = {
    (knockInOnEqual, reverse) match {
      case (true, true) => p >= trig
      case (false, true) => p > trig
      case (true, false) => p <= trig
      case (false, false) => p < trig
    }
  }

  def isKnockedInPrice(p:UnderlyingFixing, trig:UnderlyingFixing):Boolean = {
    trig.getDecimalValue.exists{case (ul, t) => p.getDecimalValue.get(ul).collect{case f => isKnockedInPrice(f, t)}.getOrElse(false)}
  }

  def getPerformance(p:Double, stk:Double):Double = {
    withMinMax(1.0 - leverage * (stk - p) / stk)
  }

  def priceSingle(priceResult:PriceResult):Double = {
    getFixedKnockIn match {
      case Some(true) if physical =>
        assignPhysicalInfo(priceResult)
        Double.NaN

      case Some(false) => 1.0

      case _ => Double.NaN

    }
  }

  def priceList(fixings:UnderlyingFixing, priceResult:PriceResult):Double = {
    if (physical) {
      getFixedKnockIn match {
        case Some(ki) =>
          if (ki) assignPhysicalInfo(priceResult)
          priceList(fixings, ki, priceResult)

        case None => Double.NaN
      }
    }
    else priceList(fixings, isKnockIn(fixings), priceResult)
  }

  def priceList(fixings:List[UnderlyingFixing], priceResult:PriceResult):Double = {
    fixings.lastOption match {
      case Some(lastFixing) =>
        if (physical) {
          val fixingSize = fixings.length

          getFixedKnockIn match {
            case Some(ki) =>
              if (ki) assignPhysicalInfo(priceResult)
              priceList(lastFixing, ki, priceResult)

            case None if fixingSize >= 2 =>
              val obsKi = isKnockIn(fixings.dropRight(1))
              if (priceResult != null && obsKi) assignPhysicalInfo(fixings.last, priceResult)
              priceList(lastFixing, obsKi, priceResult)

            case _ => Double.NaN
          }
        }
        else priceList(lastFixing, isKnockIn(fixings), priceResult)
      case None => Double.NaN
    }
  }

  private def getFixedKnockIn:Option[Boolean] = {
    if (isFixed) Some(isKnockIn(getFixings))
    else None
  }

  def isKnockIn(fixingList:List[UnderlyingFixing]):Boolean = {
    fixingList.lastOption match {
      case None => knockedIn
      case Some(lastFixings) => knockInDuringObservation(fixingList) && knockInAtRedemption(lastFixings)
    }
  }

  def isKnockIn(fixings:UnderlyingFixing):Boolean = {
    isKnockIn(List(fixings))
  }

  def knockInDuringObservation(fixingList:List[UnderlyingFixing]):Boolean = {
    knockedIn || fixingList.exists(fixings => triggerVariables.exists(ul => (fixings.getDecimalValue.get(ul), triggers.getDecimalValue.get(ul)) match {
      case (Some(v), Some(trig)) => isKnockedInPrice(v, trig)
      case _ => false
    }))
  }

  def knockInAtRedemption(fixings:UnderlyingFixing):Boolean = {
//    forward || strikeOrFinalTriggerVariables.exists(ul => (fixings.getDecimalValue.get(ul), strikeOrFinalTriggers.getDecimalValue.get(ul)) match {
//      case (Some(v), Some(stk)) => isKnockedInPrice(v, stk)
//      case _ => false
//    })
    forward || finalTriggerBasketType.isKnockedInPrice(fixings, strikeOrFinalTriggers, (v:BigDecimal, stk:BigDecimal) => isKnockedInPrice(v, stk))
  }


  def priceList(fixings:UnderlyingFixing, isKnockedIn:Boolean, priceResult:PriceResult):Double = {
    if (isPriceable && fixings.isValidFor(strikeVariables)) {
      if (isKnockedIn) {
        if (physical && priceResult != null) {
          strikeVariables.map(ul => (ul, getPerformance(fixings.getDouble(ul), strikes.getDouble(ul)), strikes.getDouble(ul))).minBy{case (ul, perf, k) => perf} match {
            case (ul, pf, k) =>
              //priceResult.setAssetInfo(ul, 1.0 / k)
              withMinMax(pf)
          }
        } else {
          withMinMax(strikeVariables.map(v => getPerformance(fixings.getDouble(v), strikes.getDouble(v))).min)
        }
      }
      else 1.0
    } else Double.NaN
  }

  def assignPhysicalInfo(priceResult:PriceResult):Unit = {
    if (isFixed) assignPhysicalInfo(getFixings, priceResult)
  }

  def assignPhysicalInfo(fixings:UnderlyingFixing, priceResult:PriceResult):Unit = {
    if (priceResult != null && fixings.isValidFor(strikeVariables)) {
      strikeVariables.map(ul => (ul, getPerformance(fixings.getDouble(ul), strikes.getDouble(ul)), strikes.getDouble(ul))).minBy{case (ul, perf, k) => perf} match {
        case (ul, pf, k) => priceResult.setAssetInfo(ul, 1.0 / k)
      }
    }
  }

  override def priceImpl(fixings:List[UnderlyingFixing], pastPayments:List[Double], priceResult:PriceResult):Double = priceList(fixings, priceResult)

  override def priceImpl(priceResult:PriceResult) = priceSingle(priceResult) //implicitly[FixingInterpreter[Map[String, Double]]] priceSingle(priceResult)

  override def toString =
    nominal.asPercent + " [" + triggers + "](Amer) " + nominal.asPercent +
    " x Min([" + variables.mkString(",") + "] / [" + strikes + "])"
  
  override def jsonMapImpl = Map(
    "type" -> "putdiamerican", 
    "variable" -> (triggers.keySet ++ strikes.keySet ++ finalTriggers.keySet).toArray,
    "trigger" -> triggers.getDouble.map{case (ul, v) => (ul, v)}.asJava,
    "strike" -> strikes.getDouble.map{case (ul, v) => (ul, v)}.asJava,
    "final_trigger" -> finalTriggers.getDouble.map{case (ul, v) => (ul, v)}.asJava,
    "final_trigger_ref" -> finalTriggerBasketType.toString,
    "leverage" -> leverage,
    "refstart" -> (if (refstart == null) null else refstart.toString),
    "refend" -> (if (refend == null) null else refend.toString),
    "description" -> description
  )

  override def fixedConditions:Map[String, Any] = {
    Map(
      "trigger" -> triggers.getDouble.map{case (ul, v) => (ul, v)}.asJava,
      "strike" -> strikes.getDouble.map{case (ul, v) => (ul, v)}.asJava,
      "final_trigger" -> finalTriggers.getDouble.map{case (ul, v) => (ul, v)}.asJava
    )
  }


  override def clearFixings = {
    super.clearFixings
    knockedIn = false
  }
    
  override def assignFixings(f:UnderlyingFixing):Unit = {
    super.assignFixings(f)
    checkKnockIn
  }

  def checkKnockIn:Unit = {
    knockedIn = checkKnockInFromDb
  }

  def historicalPriceForKnockin:Map[Date, UnderlyingFixing] = {
    val triggerUnderlyingIds = triggers.keySet
    val closeFixings:Map[Date, UnderlyingFixing] = DB.getHistoricalUnderlyingFixings(triggerUnderlyingIds, refstart, refend)

    val nonCloseFixings:Set[String] = fixingInfo.underlyingFixingPage.filter{case (k, v) => !v.isCloseFixing}.keySet & triggerUnderlyingIds

    val finalFixings:Map[Date, UnderlyingFixing] = {
      if (!nonCloseFixings.isEmpty && closeFixings.contains(refend)) {
        val newFixings = closeFixings(refend).getDecimalValue ++ getFixings.getDecimalValue.filter{case (k, v) => nonCloseFixings.contains(k)}
        Map(refend -> UnderlyingFixing(newFixings))
      }
      else Map.empty
    }

    if (closeOnly) closeFixings ++ finalFixings
    else if (reverse) closeFixings ++ DB.getHistoricalHighUnderlyingFixings(triggerUnderlyingIds, refstart, refend) ++ finalFixings
    else closeFixings ++ DB.getHistoricalLowUnderlyingFixings(triggerUnderlyingIds, refstart, refend) ++ finalFixings
  }

  def checkKnockInFromDb:Boolean = {
    if (refstart == null || refend == null) false
    else {
      val historicalPrices:Map[Date, UnderlyingFixing] = historicalPriceForKnockin
      val refPrices:Map[Date, UnderlyingFixing] = {
        if (refDates.isEmpty) historicalPrices
        else historicalPrices.filter{case (d, _) => refDates.contains(d)}
      }

      (refPrices, historicalPrices.get(refend)) match {
        case (hs, _) if hs.isEmpty => false

        case (hs, Some(hsLast)) =>
          hs.values.exists(hp => isKnockedInPrice(hp, triggers)) &&
            (
              forward || isKnockedInPrice(hsLast, strikeOrFinalTriggers)
              )

        case (hs, _) => hs.exists { case (_, x) => isKnockedInPrice(x, triggers)}
      }
    }
  }


  override def dateShifted(shift:Int):Payoff = PutDIAmericanPayoff(
    triggers = triggers,
    strikes = strikes,
    finalTriggers = finalTriggers,
    finalTriggerBasketType = finalTriggerBasketType,
    refstart = (if (refstart == null) null else refstart.add(shift)),
    refend = (if (refend == null) null else refend.add(shift)),
    refDates = refDates.map(d => d.add(shift)),
    knockedIn = knockedIn,
    physical = physical,
    forward = forward,
    closeOnly = closeOnly,
    reverse = reverse,
    leverage = leverage,
    minPayoff = minPayoff,
    maxPayoff = maxPayoff,
    knockInOnEqual = knockInOnEqual,
    amount = amount,
    description = description,
    inputString = inputString
  )

}

object PutDIAmericanPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):PutDIAmericanPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)
    val fixedNode = fixed.jsonNode

    val variables:List[String] = formula.parseJsonStringList("variable").map(_.orNull)

    val triggers = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "trigger", variables).getOptionalDecimal()(fixingInfo.getStrikeFixingInformation)}.getOrElse(Map.empty)

    val strikes = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "strike", variables).getOptionalDecimal()(fixingInfo.getStrikeFixingInformation)}.getOrElse(Map.empty)

    val finalTriggers =
      fixedNode.collect{case n =>
        if (n.has("final_trigger")) strikes ++ Payoff.nodeToComputedMap(n, "final_trigger", variables).getOptionalDecimal()(fixingInfo.getStrikeFixingInformation)
        else strikes
      }.getOrElse(Map.empty)

    val finalTriggerBasketType:BasketType= formula.parseJsonString("final_trigger_basket").collect{case s => BasketType.parseString(s)}.getOrElse(WorstOf)

    val amount:Double = fixed.parseJsonDouble("amount").getOrElse(1.0)
    val refstart:Date = formula.parseJsonDate("refstart").orNull
    val refend:Date = formula.parseJsonDate("refend").orNull
    val refDates:Set[Date] = formula.parseJsonDateList("refdates").flatMap(d => d).toSet
    val physical:Boolean = formula.parseJsonString("physical").getOrElse("0") == "1"
    val forward:Boolean = formula.parseJsonString("forward").getOrElse("0") == "1"
    val reverse:Boolean = formula.parseJsonString("reverse").getOrElse("0") == "1"
    val leverage:Double = fixed.parseJsonDouble("leverage").getOrElse(1.0)
    val minPayoff:Double = fixed.parseJsonDouble("min").getOrElse(0.0)
    val maxPayoff:Option[Double] = fixed.parseJsonDouble("max")
    val description:String = formula.parseJsonString("description").orNull
    val closeOnly:Boolean = formula.parseJsonString("reftype").getOrElse("closing") != "continuous"
    val knockInOnEqual:Boolean = formula.parseJsonString("ki_on_equal").getOrElse("1") == "1"
    
    val knockedIn:Boolean = false
    
    PutDIAmericanPayoff(
      triggers = UnderlyingFixing(triggers),
      strikes = UnderlyingFixing(strikes),
      finalTriggers = UnderlyingFixing(finalTriggers),
      finalTriggerBasketType = finalTriggerBasketType,
      refstart = refstart,
      refend = refend,
      refDates = refDates,
      knockedIn = knockedIn,
      physical = physical,
      forward = forward,
      closeOnly = closeOnly,
      reverse = reverse,
      leverage = leverage,
      minPayoff = minPayoff,
      maxPayoff = maxPayoff,
      amount = amount,
      knockInOnEqual = knockInOnEqual,
      description = description,
      inputString = inputString
    )
  }
  
}

