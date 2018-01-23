package net.squantlib.schedule.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.map.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import java.util.{Map => JavaMap}
import net.squantlib.util.FixingInformation
import net.squantlib.util.Date
import net.squantlib.schedule.CalculationPeriod
import scala.reflect.ClassTag
import org.codehaus.jackson.JsonNode


/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 *  {type:"rangeforward", variable:String, triggerlow:Double, triggerhigh:Double, strike:Double, description:String},
 * No strike is considered as no low boundary
 */
case class RangeForwardPayoff(
    triggerLow:Map[String, Double],
    triggerHigh:Map[String, Double],
    strikes:Map[String, Double],
    var knockedIn:Boolean,
    override val physical:Boolean,
    forwardInRange:Boolean = true,
    amount:Double = 1.0,
    description:String = null,
    inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {

  val variables = triggerLow.keySet ++ triggerHigh.keySet ++ strikes.keySet

  nominal = amount

  val strikeVariables:Set[String] = strikes.keySet
  val triggerVariables:Set[String] = triggerLow.keySet ++ triggerHigh.keySet

  override val isPriceable:Boolean =
    triggerHigh.forall{case (k, v) => !v.isNaN && !v.isInfinity} &&
    triggerLow.forall{case (k, v) => !v.isNaN && !v.isInfinity} &&
    strikes.forall{case (k, v) => !v.isNaN && !v.isInfinity} &&
    !strikes.isEmpty

  override def eventDates(period:CalculationPeriod):List[Date] = {
    if (!isPriceable) List(period.endDate)
    else if (physical) List(period.eventDate, period.paymentDate)
    else List(period.eventDate)
  }

  trait FixingInterpreter[T] {
    def isKnockIn(fixings:T):Boolean // Method to be implemented
    def price(fixings:T, isKnockedIn:Boolean):Double // Method to be implemented

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
            else if (fixingSize >= 2) price(lastFixing, isKnockIn(fixings(fixings.length - 2)))
            else Double.NaN
          }
          else price(lastFixing, isKnockIn(lastFixing))
        case None => Double.NaN
      }
    }

//    def priceSingle(fixing:Double, isKnockedIn:Boolean):Double = {
//      if (variables.size != 1) Double.NaN
//      else if (!isKnockedIn) 1.0
//      else if (fixing.isNaN || fixing.isInfinity || variables.size != 1 || !isPriceable) Double.NaN
//      else strikes.head match {
//        case Some(v) => fixing / v
//        case _ => Double.NaN
//      }
//    }
//
//    def isKnockInSingle(fixing:Double):Boolean = {
//      if (knockedIn || variables.size != 1) true
//      else {
//        val r = (triggerLow.head, triggerHigh.head) match {
//          case (Some(l), Some(h)) => fixing >= l && fixing <= h
//          case (None, Some(h)) => fixing <= h
//          case (Some(l), None) => fixing >= l
//          case (None, None) => true
//        }
//        if (forwardInRange) r else !r
//      }
//    }
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
  }

  implicit object MapInterpreter extends FixingInterpreter[Map[String, Double]] {

    private def validFixings(fixings:Map[String, Double], vs:Set[String]):Boolean =
      vs.subsetOf(fixings.keySet) &&
      vs.forall(v => !fixings(v).isNaN && !fixings(v).isInfinity) &&
      isPriceable

    override def isKnockIn(fixings:Map[String, Double]):Boolean = {
      if (knockedIn) true
      else if (validFixings(fixings, triggerVariables)) {
        val r = triggerLow.forall{case (ul, v) => fixings(ul) >= v} && triggerHigh.forall{case (ul, v) => fixings(ul) <= v}
        if (forwardInRange) r else !r
      }
      else true
    }

    def price(fixings:Map[String, Double], isKnockedIn:Boolean):Double = {
      if (!isKnockedIn) 1.0
      else if (validFixings(fixings, strikeVariables)) strikeVariables.map(v => fixings(v) / strikes(v)).min
      else Double.NaN
    }

  }

  implicit object DoubleInterpreter extends FixingInterpreter[Double] {

    override def isKnockIn(fixing:Double):Boolean = {
      if (variables.size > 1) true
      else {
        val r = triggerLow.values.headOption.collect{case v => fixing >= v}.getOrElse(true) &&
          triggerHigh.values.headOption.collect{case v => fixing <= v}.getOrElse(true)
        if (forwardInRange) r else !r
      }
    }

    override def price(fixing:Double, isKnockedIn:Boolean):Double = {
      if (!isKnockedIn) 1.0
      else if (variables.size != 1 || strikes.size != 1 || fixing.isNaN || fixing.isInfinity) Double.NaN
      else fixing / strikes.values.head
    }
  }

  def priceSingle[A:FixingInterpreter](fixings:A):Double = implicitly[FixingInterpreter[A]] price fixings

  def priceList[A:FixingInterpreter](fixings:List[A]):Double = implicitly[FixingInterpreter[A]] price fixings

  override def priceImpl(fixings:List[Map[String, Double]]):Double = priceList(fixings)

  override def priceImpl(fixings:Map[String, Double]):Double = priceSingle(fixings)

  override def priceImpl[T:ClassTag](fixings:List[Double]):Double = priceList(fixings)

  override def priceImpl(fixing:Double):Double = priceSingle(fixing)

  override def priceImpl = Double.NaN

  override def clearFixings = {
    super.clearFixings
    knockedIn = false
  }

  override def assignFixings(f:Map[String, Double]):Unit = {
    super.assignFixings(f)
    checkKnockIn
  }

  def checkKnockIn:Unit = {
    knockedIn = implicitly[FixingInterpreter[Map[String, Double]]] isKnockIn(getFixings)
  }



//  override def priceImpl(fixings:Map[String, Double]) =
//    fixings.get(variable) match {
//      case Some(v) if !v.isNaN && !v.isInfinity => priceImpl(v)
//      case None => Double.NaN
//    }
//
//  private def satisfyRange(fixing:Double):Boolean = {
//    val r = (triggerLow, triggerHigh) match {
//      case (Some(l), Some(h)) => fixing >= l && fixing <= h
//      case (None, Some(h)) => fixing <= h
//      case (Some(l), None) => fixing >= l
//      case (None, None) => true
//    }
//    if (forwardInRange) r else !r
//  }
//
//  override def priceImpl(fixing:Double):Double =
//    if (satisfyRange(fixing)) fixing / strike
//    else 1.0
//
//  override def clearFixings = {
//    super.clearFixings
//    knockedIn = false
//  }
//

  override def toString =
    nominal.asPercent +
      " [" + triggerLow.mkString(",") + "] " +
      " [" + triggerHigh.mkString(",") + "] " +
      nominal.asPercent +
      " x Min([" + variables.mkString(",") + "] / [" + strikes.mkString(",") + "])"

//  override def priceImpl = Double.NaN

  override def jsonMapImpl = {
    Map(
      "type" -> "rangeforward",
      "variable" -> (triggerLow.keySet ++ triggerHigh.keySet ++ strikes.keySet),
      "triggerlow" -> triggerLow,
      "triggerhigh" -> triggerHigh,
      "strike" -> strikes,
      "description" -> description
    )
  }

}

object RangeForwardPayoff {

  def getStringOrHash(node:JsonNode, s:String, variable:List[String])(implicit fixingInfo:FixingInformation):Map[String, Double] = {
    node.getOption(s) match {
      case Some(n) =>
        n.parseStringFields match {
          case rs if !rs.isEmpty => rs.map { case (k, v) => (k, fixingInfo.updateCompute(v)) }
            .collect { case (k, v) => (k, v.getOrElse(Double.NaN)) }
            .toMap
          case _ => n.parseDouble match {
            case Some(v) if variable.size == 1 => Map(variable.head -> v)
            case _ => Map.empty
          }
        }
      case _ => Map.empty
    }
  }

  def apply(inputString:String)(implicit fixingInfo:FixingInformation):RangeForwardPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)
    val fixedNode = fixed.jsonNode

    val variable:List[String] = formula.parseJsonStringList("variable").map(_.orNull)
    val triggerHigh:Map[String, Double] = fixedNode.collect{case n => getStringOrHash(n, "triggerhigh", variable)}.getOrElse(Map.empty)
    val triggerLow:Map[String, Double] = fixedNode.collect{case n => getStringOrHash(n, "triggerlow", variable)}.getOrElse(Map.empty)
    val strikes:Map[String, Double] = fixedNode.collect{case n => getStringOrHash(n, "strike", variable)}.getOrElse(Map.empty)

    val amount:Double = fixed.parseJsonDouble("amount").getOrElse(1.0)
    val forwardInRange:Boolean = formula.parseJsonString("range_type").getOrElse("in") != "out"
    val physical:Boolean = formula.parseJsonString("physical").getOrElse("0") == "1"
    val description:String = formula.parseJsonString("description").orNull

    RangeForwardPayoff(triggerLow, triggerHigh, strikes, false, physical, forwardInRange, amount, description, inputString)

  }

}
