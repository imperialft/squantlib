package net.squantlib.schedule.payoff

import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.util.FixingInformation
import net.squantlib.util.Date
import net.squantlib.schedule.CalculationPeriod
import scala.collection.JavaConverters._
import scala.reflect.ClassTag

class PutDIPayoff(
  override val triggers:Map[String, Double],
  override val strikes:Map[String, Double],
  initKnockedIn:Boolean,
  override val physical:Boolean,
  override val amount:Double = 1.0,
  override val description:String = null,
  override val inputString:String = null)(implicit override val fixingInfo:FixingInformation) extends PutDIAmericanPayoff(
    triggers,
    strikes,
    triggers,
    null,
    null,
    initKnockedIn,
    physical,
    false,
    true,
    amount,
    description,
    inputString
  )
{

  override val variables = triggers.keySet ++ strikes.keySet

  override val strikeOrFinalTriggers:Map[String, Double] = triggers

  override val isPriceable:Boolean =
    !triggers.isEmpty &&
    !triggers.values.exists(v => v.isNaN || v.isInfinity) &&
    !strikes.isEmpty &&
    !strikes.values.exists(v => v.isNaN || v.isInfinity)

  override def eventDates(period:CalculationPeriod):List[Date] = {
    if (!isPriceable) List(period.endDate)
    else if (physical) List(period.eventDate, period.paymentDate)
    else List(period.eventDate)
  }

  override def priceImpl(fixings:List[Map[String, Double]], pastPayments:List[Double]):Double = priceList(fixings.takeRight(2))

  override def priceImpl[T:ClassTag](fixings:List[Double], pastPayments:List[Double]):Double = priceList(fixings.takeRight(2))


  override def toString =
    nominal.asPercent + " [" + triggers.values.map(_.asDouble).mkString(",") + "](Eur) " + nominal.asPercent +
      " x Min([" + variables.mkString(",") + "] / [" + strikes.values.map(_.asDouble).mkString(",") + "])"

  override def jsonMapImpl = Map(
    "type" -> "putdiamerican",
    "variable" -> (triggers.keySet ++ strikes.keySet).toArray,
    "trigger" -> triggers.asJava,
    "strike" -> strikes.asJava,
    "description" -> description)


  override def clearFixings = {
    super.clearFixings
    knockedIn = false
  }

  override def assignFixings(f:Map[String, Double]):Unit = {
    super.assignFixings(f)
    checkKnockIn
  }

  override def checkKnockIn:Unit = {
    knockedIn = implicitly[FixingInterpreter[Map[String, Double]]] isKnockIn(getFixings)
  }

}



///**
// * Interprets JSON formula specification for sum of linear formulas with discrete range.
// * JSON format:
// *  {type:"putdi", variable:[String], trigger:[Double], strike:[Double], description:String},
// * No strike is considered as no low boundary
// */
//case class PutDIPayoff(
//    putVariables:List[String],
//    trigger:List[Double],
//    strike:List[Double],
//    var knockedIn:Boolean,
//    override val physical:Boolean,
//    amount:Double = 1.0,
//    description:String = null,
//    inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {
//
//  val variables = putVariables.toSet
//
//  nominal = amount
//
//  val strikeMap:Map[String, Double] = (putVariables zip strike) (collection.breakOut)
//
//  val triggerMap:Map[String, Double] = (putVariables zip trigger) (collection.breakOut)
//
//  override val isPriceable:Boolean = !trigger.exists(v => v.isNaN || v.isInfinity) && !strike.exists(v => v.isNaN || v.isInfinity)
//
//  override def eventDates(period:CalculationPeriod):List[Date] = {
//    if (!isPriceable) List(period.endDate)
//    else if (physical) List(period.eventDate, period.paymentDate)
//    else List(period.eventDate)
//  }
//
//  trait FixingInterpreter[T] {
//    def isKnockIn(fixings:T):Boolean // Method to be implemented
//    def price(fixings:T, isKnockedIn:Boolean):Double // Method to be implemented
//
//    def price(fixings:T):Double = {
//      if (physical) {
//        if (isFixed) price(fixings, knockedIn)
//        else Double.NaN
//      }
//      else price(fixings, isKnockIn(fixings))
//    }
//
//    def price(fixings:List[T]):Double = {
//      fixings.lastOption match {
//        case Some(lastFixing) =>
//          if (physical) {
//            val fixingSize = fixings.length
//            if (isFixed) price(lastFixing, knockedIn)
//            else if (fixingSize >= 2) price(lastFixing, isKnockIn(fixings(fixings.length - 2)))
//            else Double.NaN
//          }
//          else price(lastFixing, isKnockIn(lastFixing))
//        case None => Double.NaN
//      }
//    }
//  }
//
//  implicit object MapInterpreter extends FixingInterpreter[Map[String, Double]] {
//
//    override def isKnockIn(fixings:Map[String, Double]):Boolean = {
//      variables.exists(p => fixings.get(p) match {
//        case Some(v) if triggerMap.contains(p) => v <= triggerMap(p)
//        case _ => false
//      })
//    }
//
//    override def price(fixings:Map[String, Double], isKnockedIn:Boolean):Double = {
//      if (!isKnockedIn) 1.0
//      else if ((variables subsetOf fixings.keySet) && variables.forall(v => !fixings(v).isNaN && !fixings(v).isInfinity) && isPriceable) variables.map(v => fixings(v) / strikeMap(v)).min
//      else Double.NaN
//    }
//  }
//
//  implicit object DoubleInterpreter extends FixingInterpreter[Double] {
//
//    override def isKnockIn(fixing:Double):Boolean = fixing <= trigger.head
//
//    override def price(fixing:Double, isKnockedIn:Boolean):Double = {
//      if (!isKnockedIn) 1.0
//      else if (fixing.isNaN || fixing.isInfinity || variables.size != 1 || !isPriceable) Double.NaN
//      else fixing / strike.head
//    }
//  }
//
//  def priceSingle[A:FixingInterpreter](fixings:A):Double = implicitly[FixingInterpreter[A]] price fixings
//
//  def priceList[A:FixingInterpreter](fixings:List[A]):Double = implicitly[FixingInterpreter[A]] price fixings
//
//  override def priceImpl(fixings:List[Map[String, Double]]):Double = priceList(fixings)
//
//  override def priceImpl(fixings:Map[String, Double]):Double = priceSingle(fixings)
//
//  override def priceImpl[T:ClassTag](fixings:List[Double]):Double = priceList(fixings)
//
//  override def priceImpl(fixing:Double):Double = priceSingle(fixing)
//
//  override def priceImpl = Double.NaN
//
//  override def clearFixings = {
//    super.clearFixings
//    knockedIn = false
//  }
//
//  override def assignFixings(f:Map[String, Double]):Unit = {
//    super.assignFixings(f)
//    checkKnockIn
//  }
//
//  def checkKnockIn:Unit = {
//    knockedIn = implicitly[FixingInterpreter[Map[String, Double]]] isKnockIn(getFixings)
//  }
//
//
////  def getFixings(fixings:Map[String, Double]):Option[List[Double]] = {
////    if (variables.toSet subsetOf fixings.keySet)
////      Some((0 to putVariables.size - 1).toList.map(i => fixings(putVariables(i))))
////    else None
////  }
////
////  override def priceImpl(fixings:Map[String, Double]) = {
////    getFixings(fixings) match {
////      case Some(fixValues) if fixValues.forall(v => !v.isNaN && !v.isInfinity) =>
////        if (fixValues.corresponds(trigger) {_ >= _}) 1.0
////        else math.min(1.00, (fixValues, strike).zipped.map((v, k) => v/k).min)
////      case None => Double.NaN
////    }
////  }
////
////  override def priceImpl(fixing:Double) = {
////    if (variables.size != 1 || fixing.isNaN || fixing.isInfinity) Double.NaN
////    else if (fixing >= trigger.head) 1.0
////    else math.min(1.00, fixing / strike.head)
////  }
//
//  override def toString =
//    nominal.asPercent + " [" + trigger.mkString(",") + "] " + nominal.asPercent + " x Min([" + variables.mkString(",") + "] / [" + strike.mkString(",") + "])"
//
////  override def priceImpl = Double.NaN
//
//  override def jsonMapImpl = Map(
//    "type" -> "putdi",
//    "variable" -> putVariables.toArray,
//    "trigger" -> trigger.toArray,
//    "strike" -> strike.toArray,
//    "description" -> description)
//
//
//}

object PutDIPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):PutDIPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)
    val fixedNode = fixed.jsonNode

    val variables:List[String] = formula.parseJsonStringList("variable").map(_.orNull)
    val triggers:Map[String, Double] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "trigger", variables)}.getOrElse(Map.empty)
    val strikes:Map[String, Double] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "strike", variables)}.getOrElse(Map.empty)

    val amount:Double = fixed.parseJsonDouble("amount").getOrElse(1.0)
    val description:String = formula.parseJsonString("description").orNull
    val physical:Boolean = formula.parseJsonString("physical").getOrElse("0") == "1"
    val knockedIn:Boolean = false

    new PutDIPayoff(triggers, strikes, knockedIn, physical, amount, description, inputString)
  }
  
}

