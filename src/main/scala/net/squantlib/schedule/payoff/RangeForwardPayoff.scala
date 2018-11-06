package net.squantlib.schedule.payoff

import org.codehaus.jackson.map.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils
import net.squantlib.util.JsonUtils._
import net.squantlib.util.FixingInformation
import net.squantlib.util.Date
import net.squantlib.schedule.CalculationPeriod
import scala.reflect.ClassTag
import org.codehaus.jackson.JsonNode
import scala.collection.JavaConverters._

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
  inputString:String = null
)(implicit val fixingInfo:FixingInformation) extends Payoff {

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
    def isKnockIn(fixings:T):Option[Boolean] // Method to be implemented
    def price(fixings:T, isKnockedIn:Boolean, priceResult:PriceResult):Double // Method to be implemented

    def price(fixings:T, priceResult:PriceResult):Double = {
      if (physical) {
        if (isFixed) price(fixings, knockedIn, priceResult)
        else Double.NaN
      }
      else isKnockIn(fixings) match {
        case Some(ki) => price(fixings, ki, priceResult)
        case _ => Double.NaN
      }
    }

    def price(fixings:List[T], priceResult:PriceResult):Double = {
      fixings.lastOption match {
        case Some(lastFixing) =>
          if (physical) {
            val fixingSize = fixings.length
            if (isFixed) price(lastFixing, knockedIn, priceResult)
            else if (fixingSize >= 2) isKnockIn(fixings(fixings.length - 2)) match {
              case Some(ki) => price(lastFixing, ki, priceResult)
              case _ => Double.NaN
            }
            else Double.NaN
          }
          else isKnockIn(lastFixing) match {
            case Some(ki) => price(lastFixing, ki, priceResult)
            case _ => Double.NaN
          }
        case None => Double.NaN
      }
    }
  }

  implicit object MapInterpreter extends FixingInterpreter[Map[String, Double]] {

    private def validFixings(fixings:Map[String, Double], vs:Set[String]):Boolean =
      vs.subsetOf(fixings.keySet) &&
      vs.forall(v => !fixings(v).isNaN && !fixings(v).isInfinity) &&
      isPriceable

    override def isKnockIn(fixings:Map[String, Double]):Option[Boolean] = {
      if (knockedIn) Some(true)
      else if (validFixings(fixings, triggerVariables)) {
        val r = triggerLow.forall{case (ul, v) => fixings(ul) >= v} && triggerHigh.forall{case (ul, v) => fixings(ul) <= v}
        if (forwardInRange) Some(r) else Some(!r)
      }
      else {
        None
      }
    }

    def price(fixings:Map[String, Double], isKnockedIn:Boolean, priceResult:PriceResult):Double = {
      if (!isKnockedIn) 1.0
      else if (validFixings(fixings, strikeVariables)) {
        if (physical && priceResult != null) {
          strikeVariables.map(ul => (ul, fixings(ul) / strikes(ul), strikes(ul))).minBy{case (ul, perf, k) => perf} match {
            case (ul, pf, k) =>
              priceResult.setAssetInfo(ul, 1.0 / k)
              pf
          }
        } else strikeVariables.map(v => fixings(v) / strikes(v)).min
        //strikeVariables.map(v => fixings(v) / strikes(v)).min
      }
      else Double.NaN
    }

  }

  def priceList[A:FixingInterpreter](fixings:List[A], priceResult:PriceResult):Double = implicitly[FixingInterpreter[A]] price(fixings, priceResult)

  override def priceImpl(fixings:List[Map[String, Double]], pastPayments:List[Double], priceResult:PriceResult):Double = priceList(fixings, priceResult)

  override def priceImpl(priceResult:PriceResult) = Double.NaN

  override def clearFixings = {
    super.clearFixings
    knockedIn = false
  }

  override def assignFixings(f:Map[String, Double]):Unit = {
    super.assignFixings(f)
    checkKnockIn
  }

  def checkKnockIn:Unit = {
    knockedIn = (implicitly[FixingInterpreter[Map[String, Double]]] isKnockIn(getFixings)).getOrElse(false)
  }

  override def toString =
    nominal.asPercent +
      " [" + triggerLow.mkString(",") + "] " +
      " [" + triggerHigh.mkString(",") + "] " +
      nominal.asPercent +
      " x Min([" + strikeVariables.mkString(",") + "] / [" + strikes.mkString(",") + "])"

//  override def priceImpl = Double.NaN

  override def jsonMapImpl = {
    Map(
      "type" -> "rangeforward",
      "variable" -> (triggerLow.keySet ++ triggerHigh.keySet ++ strikes.keySet).asJava,
      "triggerlow" -> triggerLow.asJava,
      "triggerhigh" -> triggerHigh.asJava,
      "strike" -> strikes.asJava,
      "description" -> description
    )
  }

}

object RangeForwardPayoff {

  def apply(inputString:String)(implicit fixingInfo:FixingInformation):RangeForwardPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)
    val fixedNode = fixed.jsonNode

    val variable:List[String] = formula.parseJsonStringList("variable").map(_.orNull)
    val triggerHigh:Map[String, Double] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "triggerhigh", variable)}.getOrElse(Map.empty)
    val triggerLow:Map[String, Double] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "triggerlow", variable)}.getOrElse(Map.empty)
    val strikes:Map[String, Double] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "strike", variable)}.getOrElse(Map.empty)

    val amount:Double = fixed.parseJsonDouble("amount").getOrElse(1.0)
    val forwardInRange:Boolean = formula.parseJsonString("range_type").getOrElse("in") != "out"
    val physical:Boolean = formula.parseJsonString("physical").getOrElse("0") == "1"
    val description:String = formula.parseJsonString("description").orNull

    RangeForwardPayoff(triggerLow, triggerHigh, strikes, false, physical, forwardInRange, amount, description, inputString)

  }

}
