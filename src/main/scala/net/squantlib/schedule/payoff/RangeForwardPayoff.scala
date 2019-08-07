package net.squantlib.schedule.payoff

import com.fasterxml.jackson.databind.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils
import net.squantlib.util.JsonUtils._
import net.squantlib.util.FixingInformation
import net.squantlib.util.{Date, UnderlyingFixing}
import net.squantlib.schedule.CalculationPeriod
import scala.reflect.ClassTag
import com.fasterxml.jackson.databind.JsonNode
import scala.collection.JavaConverters._

/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 *  {type:"rangeforward", variable:String, triggerlow:Double, triggerhigh:Double, strike:Double, description:String},
 * No strike is considered as no low boundary
 */
case class RangeForwardPayoff(
  triggerLow:UnderlyingFixing,
  triggerHigh:UnderlyingFixing,
  strikes:UnderlyingFixing,
  var knockedIn:Boolean,
  override val physical:Boolean,
  forwardInRange:Boolean = true,
  amount:Double = 1.0,
  description:String = null,
  inputString:String = null
)(implicit val fixingInfo:FixingInformation) extends Payoff {

  val variables = triggerLow.keySet ++ triggerHigh.keySet ++ strikes.keySet

  nominal = amount

  val triggerVariables:Set[String] = triggerLow.keySet ++ triggerHigh.keySet
  val strikeVariables:Set[String] = strikes.keySet

  override val isPriceable:Boolean = !strikes.isEmpty && triggerLow.getDecimal.values.forall(_.isDefined) && triggerHigh.getDecimal.values.forall(_.isDefined) && strikes.getDecimal.values.forall(_.isDefined)

  override def eventDates(period:CalculationPeriod):List[Date] = {
    if (!isPriceable) List(period.endDate)
    else if (physical) List(period.eventDate, period.paymentDate)
    else List(period.eventDate)
  }

    def priceEmpty(priceResult:PriceResult):Double = {
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
      else isKnockIn(fixings) match {
        case Some(ki) => {
          priceList(fixings, ki, priceResult)
        }
        case _ => Double.NaN
      }
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
            else if (fixingSize >= 2) isKnockIn(fixings(fixings.length - 2)) match {
              case Some(ki) =>
                if (ki && priceResult != null) assignPhysicalInfo(fixings(fixings.length - 2), priceResult)
                priceList(lastFixing, ki, priceResult)
              case _ => Double.NaN
            }
            else Double.NaN
          }
          else isKnockIn(lastFixing) match {
            case Some(ki) => priceList(lastFixing, ki, priceResult)
            case _ => Double.NaN
          }
        case None => Double.NaN
      }
    }
//  }
//
//  implicit object MapInterpreter extends FixingInterpreter[Map[String, Double]] {

    private def validFixings(fixings:UnderlyingFixing, vs:Set[String]):Boolean = isPriceable && fixings.isValidFor(vs)

    def isKnockIn(fixings:UnderlyingFixing):Option[Boolean] = {
      if (knockedIn) Some(true)
      else if (validFixings(fixings, triggerVariables)) {
        val r = triggerLow.getDecimalValue.forall{case (ul, v) => v <= fixings.getDecimalValue(ul)} && triggerHigh.getDecimalValue.forall{case (ul, v) => v >= fixings.getDecimalValue(ul)}
        if (forwardInRange) Some(r) else Some(!r)
      }
      else {
        None
      }
    }

    def priceList(fixings:UnderlyingFixing, isKnockedIn:Boolean, priceResult:PriceResult):Double = {
      if (!isKnockedIn) 1.0
      else if (validFixings(fixings, strikeVariables)) {
        if (physical && priceResult != null) {
          strikeVariables.map(ul => (ul, fixings.getDouble(ul) / strikes.getDouble(ul), strikes.getDouble(ul))).minBy{case (ul, perf, k) => perf} match {
            case (ul, pf, k) =>
              //priceResult.setAssetInfo(ul, 1.0 / k)
              pf
          }
        } else strikeVariables.map(v => fixings.getDouble(v) / strikes.getDouble(v)).min
        //strikeVariables.map(v => fixings(v) / strikes(v)).min
      }
      else Double.NaN
    }

    def assignPhysicalInfo(priceResult:PriceResult):Unit = {
      if (isFixed) assignPhysicalInfo(getFixings, priceResult)
    }

    def assignPhysicalInfo(fixings:UnderlyingFixing, priceResult:PriceResult):Unit = {
      if (priceResult != null && fixings.isValidFor(strikeVariables)) {
        strikeVariables.map(ul => (ul, fixings.getDouble(ul) / strikes.getDouble(ul), strikes.getDouble(ul))).minBy{case (ul, perf, k) => perf} match {
          case (ul, pf, k) => priceResult.setAssetInfo(ul, 1.0 / k)
        }
      }
    }

//  }

//  def priceList[A:FixingInterpreter](fixings:List[A], priceResult:PriceResult):Double = implicitly[FixingInterpreter[A]] price(fixings, priceResult)
//
  override def priceImpl(fixings:List[UnderlyingFixing], pastPayments:List[Double], priceResult:PriceResult):Double = priceList(fixings, priceResult)
//
  override def priceImpl(priceResult:PriceResult) = priceEmpty(priceResult)

  override def clearFixings = {
    super.clearFixings
    knockedIn = false
  }

  override def assignFixings(f:UnderlyingFixing):Unit = {
    super.assignFixings(f)
    checkKnockIn
  }

  def checkKnockIn:Unit = {
    knockedIn = isKnockIn(getFixings).getOrElse(false) //(implicitly[FixingInterpreter[Map[String, Double]]] isKnockIn(getFixings)).getOrElse(false)
  }

  override def toString =
    nominal.asPercent +
      (if (triggerLow.isEmpty) "" else  "[" + triggerLow + "] ") +
      " [" + triggerHigh + "] " +
      nominal.asPercent +
      " x Min([" + strikeVariables.mkString(",") + "] / [" + strikes + "])"

//  override def priceImpl = Double.NaN

  override def jsonMapImpl = {
    Map(
      "type" -> "rangeforward",
      "variable" -> (triggerLow.keySet ++ triggerHigh.keySet ++ strikes.keySet).asJava,
      "triggerlow" -> triggerLow.getDouble.map{case (ul, v) => (ul, v)}.asJava,
      "triggerhigh" -> triggerHigh.getDouble.map{case (ul, v) => (ul, v)}.asJava,
      "strike" -> strikes.getDouble.map{case (ul, v) => (ul, v)}.asJava,
      "description" -> description
    )
  }

  override def fixedConditions:Map[String, Any] = {
    Map(
      "triggerlow" -> triggerLow.getDouble.map{case (ul, v) => (ul, v)}.asJava,
      "triggerhigh" -> triggerHigh.getDouble.map{case (ul, v) => (ul, v)}.asJava,
      "strike" -> strikes.getDouble.map{case (ul, v) => (ul, v)}.asJava
    )
  }

}

object RangeForwardPayoff {

  def apply(inputString:String)(implicit fixingInfo:FixingInformation):RangeForwardPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)
    val fixedNode = fixed.jsonNode

    val variable:List[String] = formula.parseJsonStringList("variable").map(_.orNull)
    val triggerHigh:Map[String, Option[BigDecimal]] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "triggerhigh", variable).getOptionalDecimal}.getOrElse(Map.empty)
    val triggerLow:Map[String, Option[BigDecimal]] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "triggerlow", variable).getOptionalDecimal}.getOrElse(Map.empty)
    val strikes:Map[String, Option[BigDecimal]] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "strike", variable).getOptionalDecimal}.getOrElse(Map.empty)

    val amount:Double = fixed.parseJsonDouble("amount").getOrElse(1.0)
    val forwardInRange:Boolean = formula.parseJsonString("range_type").getOrElse("in") != "out"
    val physical:Boolean = formula.parseJsonString("physical").getOrElse("0") == "1"
    val description:String = formula.parseJsonString("description").orNull

    RangeForwardPayoff(
      triggerLow = UnderlyingFixing(triggerLow),
      triggerHigh = UnderlyingFixing(triggerHigh),
      strikes = UnderlyingFixing(strikes),
      knockedIn = false,
      physical = physical,
      forwardInRange = forwardInRange,
      amount = amount,
      description = description,
      inputString = inputString
    )

  }

}
