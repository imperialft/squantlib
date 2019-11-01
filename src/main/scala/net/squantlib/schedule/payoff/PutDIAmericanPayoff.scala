package net.squantlib.schedule.payoff

import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.database.DB
import net.squantlib.util.{Date, FixingInformation, UnderlyingFixing}
import net.squantlib.schedule.CalculationPeriod

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
  triggers: UnderlyingFixing,
  strikes: UnderlyingFixing,
  finalTriggers: UnderlyingFixing,
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
      if (dates.head == refstart) dates :+ period.paymentDate else (refstart :: dates) :+ period.paymentDate
    } else {
      if (dates.head == refstart) dates else refstart :: dates
    }
  }

//  def isKnockedInPrice(ul:String, p:Double, trig:BigDecimal):Boolean = {
//    (knockInOnEqual, reverse) match {
//      case (true, true) => p ~>= (trig, ul)
//      case (false, true) => p ~> (trig, ul)
//      case (true, false) => p ~<= (trig, ul)
//      case (false, false) => p ~< (trig, ul)
//    }
//  }

  def isKnockedInPrice(p:BigDecimal, trig:BigDecimal):Boolean = {
    (knockInOnEqual, reverse) match {
      case (true, true) => p >= trig
      case (false, true) => p > trig
      case (true, false) => p <= trig
      case (false, false) => p < trig
    }

//    if (reverse) p >= trig
//    else p <= trig
  }

  def isKnockedInPrice(p:UnderlyingFixing, trig:UnderlyingFixing):Boolean = {
    trig.getDecimalValue.exists{case (ul, t) => p.getDecimalValue.get(ul).collect{case f => isKnockedInPrice(f, t)}.getOrElse(false)}
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

  def priceList(fixings:UnderlyingFixing, priceResult:PriceResult):Double = {
    if (physical) {
      if (isFixed) {
        if (knockedIn) assignPhysicalInfo(priceResult)
        priceList(fixings, knockedIn, priceResult)
      }
      else Double.NaN
    }
    else priceList(fixings, isKnockIn(fixings), priceResult)
  }

  def priceList(fixings:List[UnderlyingFixing], priceResult:PriceResult):Double = {
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

  def isKnockIn(fixings:List[UnderlyingFixing]):Boolean = {
    if (fixings.isEmpty) knockedIn
    else (knockedIn || fixings.exists(isKnockIn(_))) && (forward || knockInAtRedemption(fixings.last))
  }

  def isKnockIn(fixings:UnderlyingFixing):Boolean = {
    knockedIn || triggerVariables.exists(ul => (fixings.getDecimalValue.get(ul), triggers.getDecimalValue.get(ul)) match {
      case (Some(v), Some(trig)) => isKnockedInPrice(v, trig)
      case _ => false
    })
  }

  def knockInAtRedemption(fixings:UnderlyingFixing):Boolean = {
    strikeOrFinalTriggerVariables.exists(ul => (fixings.getDecimalValue.get(ul), strikeOrFinalTriggers.getDecimalValue.get(ul)) match {
      case (Some(v), Some(stk)) => isKnockedInPrice(v, stk)
      case _ => false
    })
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

//  }

//  def priceList[A:FixingInterpreter](fixings:List[A], priceResult:PriceResult):Double = implicitly[FixingInterpreter[A]] price(fixings, priceResult)

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
    knockedIn = {
      if (refstart == null || refend == null) false
      else {
        val historicalPrices:Map[Date, UnderlyingFixing] = {
          if (closeOnly) DB.getHistoricalUnderlyingFixings(triggers.keySet, refstart, refend)
          else if (reverse) DB.getHistoricalUnderlyingFixings(triggers.keySet, refstart, refend) ++ DB.getHistoricalHighUnderlyingFixings(triggers.keySet, refstart, refend)
          else DB.getHistoricalUnderlyingFixings(triggers.keySet, refstart, refend) ++ DB.getHistoricalLowUnderlyingFixings(triggers.keySet, refstart, refend)
        }

        (historicalPrices, historicalPrices.get(refend)) match {
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
  }


  override def dateShifted(shift:Int):Payoff = PutDIAmericanPayoff(
    triggers = triggers,
    strikes = strikes,
    finalTriggers = finalTriggers,
    refstart = refstart.add(shift),
    refend = refend.add(shift),
    knockedIn = knockedIn,
    physical = physical,
    forward = forward,
    closeOnly = closeOnly,
    reverse = reverse,
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

    val triggers = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "trigger", variables).getOptionalDecimal}.getOrElse(Map.empty)

    val strikes = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "strike", variables).getOptionalDecimal}.getOrElse(Map.empty)

    val finalTriggers =
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
      triggers = UnderlyingFixing(triggers),
      strikes = UnderlyingFixing(strikes),
      finalTriggers = UnderlyingFixing(finalTriggers),
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

