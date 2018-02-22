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
    triggers:Map[String, Double],
    strikes:Map[String, Double],
    finalTriggers:Map[String, Double],
    refstart:Date, 
    refend:Date,
    var knockedIn:Boolean,
    override val physical:Boolean,
    forward:Boolean,
    closeOnly:Boolean,
    amount:Double = 1.0, 
    description:String = null,
    inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {
  
  override val variables = triggers.keySet ++ strikes.keySet ++ finalTriggers.keySet
  
  nominal = amount
  
  val strikeOrFinalTriggers:Map[String, Double] = (strikes.keySet ++ finalTriggers.keySet).map(v => (strikes.get(v), finalTriggers.get(v)) match {
    case (Some(s), Some(t)) => (v, Math.min(s, t))
    case (None, Some(t)) => (v, t)
    case (Some(s), None) => (v, s)
    case _ => (v, Double.NaN)
  }).toMap

  override val isPriceable:Boolean =
    !triggers.isEmpty &&
    !triggers.values.exists(v => v.isNaN || v.isInfinity) &&
    !strikes.isEmpty &&
    !strikes.values.exists(v => v.isNaN || v.isInfinity) &&
    refstart != null && 
    refend != null &&
    (refstart le refend)
  
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
  
  trait FixingInterpreter[T] {
    def isKnockIn(fixings:T):Boolean // Method to be implemented
    def minBelowStrike(fixings:T):Boolean // Method to be implemented
    def price(fixings:T, isKnockedIn:Boolean):Double // Method to be implemented
    
    def isKnockIn(fixings:List[T]):Boolean = {
      if (fixings.isEmpty) knockedIn
      else (knockedIn || fixings.exists(isKnockIn(_))) && (forward || minBelowStrike(fixings.last))
    }

    def price(fixings:T):Double = {
      if (physical) {
        if (isFixed) price(fixings, knockedIn)
        else Double.NaN
      }
      else price(fixings, isKnockIn(fixings))
    }

    def price(fixings:List[T]):Double = {
      fixings.lastOption match {
        case Some(lastFixing) => 
          if (physical) {
            val fixingSize = fixings.length
            if (isFixed) price(lastFixing, knockedIn)
            else if (fixingSize >= 2) price(lastFixing, isKnockIn(fixings.dropRight(1)))
            else Double.NaN
          }
          else price(lastFixing, isKnockIn(fixings))
        case None => Double.NaN
      }
    }
  }
  
  implicit object MapInterpreter extends FixingInterpreter[Map[String, Double]] {
    
    override def isKnockIn(fixings:Map[String, Double]):Boolean = {
      knockedIn || variables.exists(p => fixings.get(p) match { 
        case Some(v) if triggers.contains(p) => v <= triggers(p)
        case _ => false
      })
    }

    override def minBelowStrike(fixings:Map[String, Double]):Boolean = {
      variables.exists(p => fixings.get(p) match { 
        case Some(v) if strikeOrFinalTriggers.contains(p) => v <= strikeOrFinalTriggers(p)
        case _ => false
      })
    }

    override def price(fixings:Map[String, Double], isKnockedIn:Boolean):Double = {
      if ((variables subsetOf fixings.keySet) && variables.forall(v => !fixings(v).isNaN && !fixings(v).isInfinity) && isPriceable) {
        if (isKnockedIn) variables.map(v => fixings(v) / strikes(v)).min
        else 1.0
      } else Double.NaN
    }

  }
  
//  implicit object DoubleInterpreter extends FixingInterpreter[Double] {
//
//    override def isKnockIn(fixing:Double):Boolean = knockedIn || triggers.values.headOption.collect{case t => fixing <= t}.getOrElse(false)
//
//    override def minBelowStrike(fixing:Double):Boolean = strikeOrFinalTriggers.values.headOption.collect{case s => fixing <= s}.getOrElse(true)
//
//    override def price(fixing:Double, isKnockedIn:Boolean):Double =
//      if (fixing.isNaN || fixing.isInfinity || variables.size != 1 || !isPriceable) Double.NaN
//      else if (physical) {
//        if (isKnockedIn) strikes.values.headOption.collect{case s => fixing / s}.getOrElse(Double.NaN)
//        else 1.0
//      }
//      else {
//        if (isKnockedIn && (forward || minBelowStrike(fixing))) strikes.values.headOption.collect{case s => fixing / s}.getOrElse(Double.NaN)
//        else 1.0
//      }
//    }
  
//  def priceSingle[A:FixingInterpreter](fixings:A):Double = implicitly[FixingInterpreter[A]] price fixings
  
  def priceList[A:FixingInterpreter](fixings:List[A]):Double = implicitly[FixingInterpreter[A]] price fixings

  override def priceImpl(fixings:List[Map[String, Double]], pastPayments:List[Double]):Double = priceList(fixings)

//  override def priceImpl(fixings:Map[String, Double], pastPayments:List[Double]):Double = priceSingle(fixings)
  
//  override def priceImpl[T:ClassTag](fixings:List[Double], pastPayments:List[Double]):Double = priceList(fixings)

//  override def priceImpl(fixing:Double, pastPayments:List[Double]):Double = priceSingle(fixing)

  override def priceImpl = Double.NaN

//  override def priceImpl(market:Market, pastPayments:List[Double]):Double = price(List.fill(2)(market.getFixings(variables)), pastPayments)

  override def toString =
    nominal.asPercent + " [" + triggers.values.map(_.asDouble).mkString(",") + "](Amer) " + nominal.asPercent +
    " x Min([" + variables.mkString(",") + "] / [" + strikes.values.map(_.asDouble).mkString(",") + "])"
  
  override def jsonMapImpl = Map(
    "type" -> "putdiamerican", 
    "variable" -> (triggers.keySet ++ strikes.keySet ++ finalTriggers.keySet).toArray,
    "trigger" -> triggers.asJava,
    "strike" -> strikes.asJava,
    "final_trigger" -> finalTriggers.asJava,
    "refstart" -> (if (refstart == null) null else refstart.toString),
    "refend" -> (if (refend == null) null else refend.toString),
    "description" -> description)
    

  override def clearFixings = {
    super.clearFixings
    knockedIn = false
  }
    
  override def assignFixings(f:Map[String, Double]):Unit = {
    super.assignFixings(f)
    checkKnockIn
  }
    
  def checkKnockIn:Unit = {
    knockedIn = 
      if (refstart == null || refend == null) false
      else triggers.exists{case (v, trig) =>
        
        val historicalPrices = if (closeOnly) DB.getHistorical(v, refstart, refend) else DB.getHistorical(v, refstart, refend) ++ DB.getHistoricalLow(v, refstart, refend)
        
        (historicalPrices, historicalPrices.values.lastOption) match {
          case (hs, _) if hs.isEmpty => false
          case (hs, Some(hsLast)) if hs.get(refend).isDefined => //implicitly[FixingInterpreter[Double]] isKnockIn(hs.values.toList)
          //    override def isKnockIn(fixing:Double):Boolean = knockedIn || triggers.values.headOption.collect{case t => fixing <= t}.getOrElse(false)
            hs.values.exists(hp => hp <= trig) && (forward || strikeOrFinalTriggers.get(v).collect{case s => hsLast <= s}.getOrElse(true))

          case (hs, _) => hs.exists{case (_, x) => x <= trig}
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

    val triggers:Map[String, Double] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "trigger", variables)}.getOrElse(Map.empty)

    val strikes:Map[String, Double] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "strike", variables)}.getOrElse(Map.empty)

    val finalTriggers:Map[String, Double] =
      fixedNode.collect{case n =>
        if (n.has("final_trigger")) strikes ++ Payoff.nodeToComputedMap(n, "final_trigger", variables)
        else strikes
      }.getOrElse(Map.empty)

    val amount:Double = fixed.parseJsonDouble("amount").getOrElse(1.0)
    val refstart:Date = formula.parseJsonDate("refstart").orNull
    val refend:Date = formula.parseJsonDate("refend").orNull
    val physical:Boolean = formula.parseJsonString("physical").getOrElse("0") == "1"
    val forward:Boolean = formula.parseJsonString("forward").getOrElse("0") == "1"
    val description:String = formula.parseJsonString("description").orNull
    val closeOnly:Boolean = formula.parseJsonString("reftype").getOrElse("closing") != "continuous"
    
    val knockedIn:Boolean = false
    
    PutDIAmericanPayoff(triggers, strikes, finalTriggers, refstart, refend, knockedIn, physical, forward, closeOnly, amount, description, inputString)
  }
  
}

