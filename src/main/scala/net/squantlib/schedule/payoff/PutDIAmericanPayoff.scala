package net.squantlib.schedule.payoff

import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.database.DB
import net.squantlib.util.Date
import net.squantlib.schedule.CalculationPeriod
import net.squantlib.util.FixingInformation
import scala.reflect.ClassTag
import net.squantlib.model.market.Market
import scala.collection.JavaConverters._


/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 *  {type:"putdiamerican", variable:[String], trigger:[Double], strike:[Double], refstart:Date, refend:Date, description:String}, 
 * No strike is considered as no low boundary
 */
case class PutDIAmericanPayoff(
  triggerDefinition:Map[String, Option[BigDecimal]],
  strikeDefinition:Map[String, Option[BigDecimal]],
  finalTriggerDefinition:Map[String, Option[BigDecimal]],
  refstart:Date,
  refend:Date,
  var knockedIn:Boolean,
  override val physical:Boolean,
  forward:Boolean,
  closeOnly:Boolean,
  reverse: Boolean,
  override val minPayoff: Double,
  override val maxPayoff: Option[Double],
  knockInOnEqual: Boolean,
  amount: Double = 1.0,
  description:String = null,
  inputString:String = null
)(implicit val fixingInfo:FixingInformation) extends Payoff {

  override val variables = triggerDefinition.keySet ++ strikeDefinition.keySet ++ finalTriggerDefinition.keySet

  val strikes = strikeDefinition.collect{case (ul, Some(v)) => (ul, v)}
  val triggers = triggerDefinition.collect{case (ul, Some(v)) => (ul, v)}
  val finalTriggers = finalTriggerDefinition.collect{case (ul, Some(v)) => (ul, v)}

  val strikeVariables = strikeDefinition.keySet
  val triggerVariables = triggerDefinition.keySet
  val strikeOrFinalTriggerVariables = finalTriggerDefinition.keySet

  val strikesDouble = strikes.map{case (ul, v) => (ul, v.toDouble)}
  
  nominal = amount
  
  val strikeOrFinalTriggers:Map[String, BigDecimal] = {
    (strikes.keySet ++ finalTriggers.keySet).map(v =>
      (strikes.get(v), finalTriggers.get(v)) match {
        case (Some(s), Some(t)) => (v, s.min(t))
        case (None, Some(t)) => (v, t)
        case (Some(s), None) => (v, s)
        case _ => (v, BigDecimal.valueOf(999999999999.0))
      }
    ).toMap
  }

  override val isPriceable:Boolean =
    !triggers.isEmpty &&
    !strikes.isEmpty &&
    refstart != null &&
    refend != null &&
    (refstart le refend) &&
    triggerDefinition.values.forall(_.isDefined) &&
    strikeDefinition.values.forall(_.isDefined) &&
    finalTriggerDefinition.values.forall(_.isDefined)

  var mcPeriod6m = 30
  var mcPeriod1y = 90
  var mcPeriodbefore = 180
  
  override def eventDates(period:CalculationPeriod):List[Date] = {
    if (!isPriceable) {return List(period.endDate)}
    val basemod = refend.serialNumber % mcPeriod6m
    val start = refstart.serialNumber
    val end = refend.serialNumber
    val dates:List[Date] = (for (i <- (start to end)
        if (i >= end - 180 && i % mcPeriod6m == basemod)
        || (i >= end - 360 && i % mcPeriod1y == basemod)
        || (i % mcPeriodbefore == basemod)) yield Date(i)) (collection.breakOut)

    if (physical) {
      if (dates.head == refstart) dates :+ period.paymentDate else (refstart :: dates) :+ period.paymentDate
    } else {
      if (dates.head == refstart) dates else refstart :: dates
    }
  }

  def isKnockedInPrice(ul:String, p:Double, trig:BigDecimal):Boolean = {
    (knockInOnEqual, reverse) match {
      case (true, true) => p ~>= (trig, ul)
      case (false, true) => p ~> (trig, ul)
      case (true, false) => p ~<= (trig, ul)
      case (false, false) => p ~< (trig, ul)
    }
  }

  def isKnockedInPrice(ul:String, p:BigDecimal, trig:BigDecimal):Boolean = {
    (knockInOnEqual, reverse) match {
      case (true, true) => p >= trig
      case (false, true) => p > trig
      case (true, false) => p <= trig
      case (false, false) => p < trig
    }

//    if (reverse) p >= trig
//    else p <= trig
  }

  def getPerformance(p:Double, stk:Double):Double = {
    if (reverse) withMinMax(2.0 - p / stk)
    else withMinMax(p / stk)
  }

  def priceSingle(priceResult:PriceResult):Double = {
    if (isFixed) {
      if (physical && knockedIn) {
        assignPhysicalInfo(priceResult)
        Double.NaN
      } else if (!knockedIn) {
        1.0
      } else Double.NaN
    }
    else Double.NaN
  }

  def priceList(fixings:Map[String, Double], priceResult:PriceResult):Double = {
    if (physical) {
      if (isFixed) {
        if (knockedIn) assignPhysicalInfo(priceResult)
        priceList(fixings, knockedIn, priceResult)
      }
      else Double.NaN
    }
    else priceList(fixings, isKnockInDouble(fixings), priceResult)
  }

  def priceList(fixings:List[Map[String, Double]], priceResult:PriceResult):Double = {
    fixings.lastOption match {
      case Some(lastFixing) =>
        if (physical) {
          val fixingSize = fixings.length
          if (isFixed) {
            if (knockedIn) assignPhysicalInfo(priceResult)
            priceList(lastFixing, knockedIn, priceResult)
          }
          else if (fixingSize >= 2) {
            val ki = isKnockIn(fixings.dropRight(1))
            if (priceResult != null && ki) assignPhysicalInfo(fixings.last, priceResult)
            priceList(lastFixing, ki, priceResult)
          }
          else Double.NaN
        }
        else priceList(lastFixing, isKnockIn(fixings), priceResult)
      case None => Double.NaN
    }
  }

  def isKnockIn(fixings:List[Map[String, Double]]):Boolean = {
    if (fixings.isEmpty) knockedIn
    else (knockedIn || fixings.exists(isKnockInDouble(_))) && (forward || knockInAtRedemptionDouble(fixings.last))
  }

  def isKnockIn(fixings:Map[String, BigDecimal]):Boolean = {
    knockedIn || triggerVariables.exists(ul => fixings.get(ul) match {
      case Some(v) if triggers.contains(ul) => isKnockedInPrice(ul, v, triggers(ul))
      case _ => false
    })
  }

  def isKnockInDouble(fixings:Map[String, Double]):Boolean = {
    knockedIn || triggerVariables.exists(ul => fixings.get(ul) match {
      case Some(v) if triggers.contains(ul) => isKnockedInPrice(ul, v, triggers(ul))
      case _ => false
    })
  }

  def knockInAtRedemption(fixings:Map[String, BigDecimal]):Boolean = {
    strikeOrFinalTriggerVariables.exists(ul => fixings.get(ul) match {
      case Some(v) if strikeOrFinalTriggers.contains(ul) => isKnockedInPrice(ul, v, strikeOrFinalTriggers(ul))
      case _ => false
    })
  }

  def knockInAtRedemptionDouble(fixings:Map[String, Double]):Boolean = {
    strikeOrFinalTriggerVariables.exists(ul => fixings.get(ul) match {
      case Some(v) if strikeOrFinalTriggers.contains(ul) => isKnockedInPrice(ul, v, strikeOrFinalTriggers(ul))
      case _ => false
    })
  }


  def priceList(fixings:Map[String, Double], isKnockedIn:Boolean, priceResult:PriceResult):Double = {
    if ((strikeVariables subsetOf fixings.keySet) && strikeVariables.forall(v => !fixings(v).isNaN && !fixings(v).isInfinity) && isPriceable) {
      if (isKnockedIn) {
        if (physical && priceResult != null) {
          strikeVariables.map(ul => (ul, getPerformance(fixings(ul), strikesDouble(ul)), strikes(ul))).minBy{case (ul, perf, k) => perf} match {
            case (ul, pf, k) =>
              //priceResult.setAssetInfo(ul, 1.0 / k)
              withMinMax(pf)
          }
        } else {
          withMinMax(strikeVariables.map(v => getPerformance(fixings(v), strikesDouble(v))).min)
        }
      }
      else 1.0
    } else Double.NaN
  }

  def assignPhysicalInfo(priceResult:PriceResult):Unit = {
    if (isFixed) assignPhysicalInfo(getDoubleFixings, priceResult)
  }

  def assignPhysicalInfo(fixings:Map[String, Double], priceResult:PriceResult):Unit = {
    if (priceResult != null && strikeVariables.subsetOf(fixings.keySet)) {
      strikeVariables.map(ul => (ul, getPerformance(fixings(ul), strikesDouble(ul)), strikesDouble(ul))).minBy{case (ul, perf, k) => perf} match {
        case (ul, pf, k) => priceResult.setAssetInfo(ul, 1.0 / k)
      }
    }
  }

//  }

//  def priceList[A:FixingInterpreter](fixings:List[A], priceResult:PriceResult):Double = implicitly[FixingInterpreter[A]] price(fixings, priceResult)

  override def priceImpl(fixings:List[Map[String, Double]], pastPayments:List[Double], priceResult:PriceResult):Double = priceList(fixings, priceResult)

  override def priceImpl(priceResult:PriceResult) = priceSingle(priceResult) //implicitly[FixingInterpreter[Map[String, Double]]] priceSingle(priceResult)

  override def toString =
    nominal.asPercent + " [" + triggers.values.map(_.asDouble).mkString(",") + "](Amer) " + nominal.asPercent +
    " x Min([" + variables.mkString(",") + "] / [" + strikes.values.map(_.asDouble).mkString(",") + "])"
  
  override def jsonMapImpl = Map(
    "type" -> "putdiamerican", 
    "variable" -> (triggerDefinition.keySet ++ strikeDefinition.keySet ++ finalTriggerDefinition.keySet).toArray,
    "trigger" -> triggerDefinition.map{case (ul, v) => (ul, v.collect{case vv => vv.toDouble}.getOrElse(Double.NaN))}.asJava,
    "strike" -> strikeDefinition.map{case (ul, v) => (ul, v.collect{case vv => vv.toDouble}.getOrElse(Double.NaN))}.asJava,
    "final_trigger" -> finalTriggerDefinition.map{case (ul, v) => (ul, v.collect{case vv => vv.toDouble}.getOrElse(Double.NaN))}.asJava,
    "refstart" -> (if (refstart == null) null else refstart.toString),
    "refend" -> (if (refend == null) null else refend.toString),
    "description" -> description
  )

  override def fixedConditions:Map[String, Any] = {
    Map(
      "trigger" -> triggerDefinition.map{case (ul, v) => (ul, v.collect{case vv => vv.toDouble}.getOrElse(Double.NaN))}.asJava,
      "strike" -> strikeDefinition.map{case (ul, v) => (ul, v.collect{case vv => vv.toDouble}.getOrElse(Double.NaN))}.asJava,
      "final_trigger" -> finalTriggerDefinition.map{case (ul, v) => (ul, v.collect{case vv => vv.toDouble}.getOrElse(Double.NaN))}.asJava
    )
  }


  override def clearFixings = {
    super.clearFixings
    knockedIn = false
  }
    
  override def assignFixings(f:Map[String, BigDecimal]):Unit = {
    super.assignFixings(f)
    checkKnockIn
  }

  def checkKnockIn:Unit = {
    knockedIn = {
      if (refstart == null || refend == null) false
      else triggers.exists { case (v, trig) =>

        val historicalPrices = {
          if (closeOnly) DB.getHistorical(v, refstart, refend)
          else if (reverse) DB.getHistorical(v, refstart, refend) ++ DB.getHistoricalHigh(v, refstart, refend)
          else DB.getHistorical(v, refstart, refend) ++ DB.getHistoricalLow(v, refstart, refend)
        }

        (historicalPrices, historicalPrices.values.lastOption) match {
          case (hs, _) if hs.isEmpty => false

          case (hs, Some(hsLast)) if hs.get(refend).isDefined =>
            hs.values.exists(hp => isKnockedInPrice(v, hp, trig)) && //hp <= trig) &&
            (
              forward ||
              strikeOrFinalTriggers.get(v).collect { case s => isKnockedInPrice(v, hsLast, s)}.getOrElse(true) //hsLast <= s }.getOrElse(true)
            )

          case (hs, _) => hs.exists { case (_, x) => isKnockedInPrice(v, x, trig)} //x <= trig }
        }
      }
    }
  }
  
}

object PutDIAmericanPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):PutDIAmericanPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)
    val fixedNode = fixed.jsonNode

    val variables:List[String] = formula.parseJsonStringList("variable").map(_.orNull)

    val triggers:Map[String, Option[BigDecimal]] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "trigger", variables).getOptionalDecimal}.getOrElse(Map.empty)

    val strikes:Map[String, Option[BigDecimal]] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "strike", variables).getOptionalDecimal}.getOrElse(Map.empty)

    val finalTriggers:Map[String, Option[BigDecimal]] =
      fixedNode.collect{case n =>
        if (n.has("final_trigger")) strikes ++ Payoff.nodeToComputedMap(n, "final_trigger", variables).getOptionalDecimal
        else strikes
      }.getOrElse(Map.empty)

    val amount:Double = fixed.parseJsonDouble("amount").getOrElse(1.0)
    val refstart:Date = formula.parseJsonDate("refstart").orNull
    val refend:Date = formula.parseJsonDate("refend").orNull
    val physical:Boolean = formula.parseJsonString("physical").getOrElse("0") == "1"
    val forward:Boolean = formula.parseJsonString("forward").getOrElse("0") == "1"
    val reverse:Boolean = formula.parseJsonString("reverse").getOrElse("0") == "1"
    val minPayoff:Double = fixed.parseJsonDouble("min").getOrElse(0.0)
    val maxPayoff:Option[Double] = fixed.parseJsonDouble("max")
    val description:String = formula.parseJsonString("description").orNull
    val closeOnly:Boolean = formula.parseJsonString("reftype").getOrElse("closing") != "continuous"
    val knockInOnEqual:Boolean = formula.parseJsonString("ki_on_equal").getOrElse("1") == "1"
    
    val knockedIn:Boolean = false
    
    PutDIAmericanPayoff(
      triggerDefinition = triggers,
      strikeDefinition = strikes,
      finalTriggerDefinition = finalTriggers,
      refstart = refstart,
      refend = refend,
      knockedIn = knockedIn,
      physical = physical,
      forward = forward,
      closeOnly = closeOnly,
      reverse = reverse,
      minPayoff = minPayoff,
      maxPayoff = maxPayoff,
      amount = amount,
      knockInOnEqual = knockInOnEqual,
      description = description,
      inputString = inputString
    )
  }
  
}

