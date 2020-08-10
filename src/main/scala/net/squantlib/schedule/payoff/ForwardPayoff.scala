package net.squantlib.schedule.payoff

import scala.collection.JavaConversions._
import com.fasterxml.jackson.databind.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.util.{Date, FixingInformation, UnderlyingFixing}
import java.util.{Map => JavaMap}

import net.squantlib.schedule.CalculationPeriod

import scala.reflect.ClassTag


/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 *  {type:"forward", variable:[String], trigger:[Double], strike:[Double], description:String}, 
 * No strike is considered as no low boundary
 */
case class ForwardPayoff(
  strikes:UnderlyingFixing,
  override val physical:Boolean,
  leverage: Double,
  override val minPayoff:Double,
  override val maxPayoff:Option[Double],
  description:String = null,
  inputString:String = null
)(implicit val fixingInfo:FixingInformation) extends Payoff {

  override val variables = strikes.keySet
  
  override val isPriceable = strikes.isPositive

  override def eventDates(period:CalculationPeriod):List[Date] = {
    if (physical) List(period.eventDate, period.paymentDate)
    else List(period.eventDate)
  }
  
  def getPerformance(p:Double, stk:Double):Double = {
    withMinMax(1.0 - leverage * (stk - p) / stk)
//    if (reverse) withMinMax(2.0 - p / stk)
//    else withMinMax(p / stk)
  }

  override def priceImpl(fixings:List[UnderlyingFixing], pastPayments:List[Double], priceResult:PriceResult):Double = {
    fixings.lastOption match {
      case Some(lastFixing) =>
        if (physical) {
          val fixingSize = fixings.length
          if (isFixed) {
            if (priceResult != null) assignPhysicalInfo(priceResult)
            priceImpl(lastFixing, pastPayments, priceResult)
          }
          else if (fixingSize >= 2) {
            if (priceResult != null) assignPhysicalInfo(fixings.dropRight(1).last, priceResult)
            priceImpl(lastFixing, pastPayments, priceResult)
          }
          else Double.NaN
        }
        else priceImpl(lastFixing, pastPayments, priceResult)

      case None => Double.NaN
    }


//    fixings.lastOption.collect {
//      case f => priceImpl(f, pastPayments, priceResult)
//    }.getOrElse(Double.NaN)
  }

  def priceImpl(fixings:UnderlyingFixing, pastPayments:List[Double], priceResult:PriceResult):Double = {
    if (isPriceable && fixings.isValidFor(variables)) {
      if (physical) {
        if (isFixed) assignPhysicalInfo(priceResult)
        variables.map(ul => (ul, getPerformance(fixings.getDouble(ul), strikes.getDouble(ul)), strikes.getDouble(ul))).minBy{case (ul, perf, k) => perf} match {
          case (ul, pf, k) =>
            withMinMax(pf)
        }
      } else {
        withMinMax(variables.map(ul => getPerformance(fixings.getDouble(ul), strikes.getDouble(ul))).min)
      }
    } else Double.NaN

  }

  def assignPhysicalInfo(priceResult:PriceResult):Unit = {
    if (isFixed) assignPhysicalInfo(getFixings, priceResult)
  }

  def assignPhysicalInfo(fixings:UnderlyingFixing, priceResult:PriceResult):Unit = {
    if (priceResult != null && fixings.isValidFor(variables)) {
      variables.map(ul => (ul, getPerformance(fixings.getDouble(ul), strikes.getDouble(ul)), strikes.getDouble(ul))).minBy{case (ul, perf, k) => perf} match {
        case (ul, pf, k) => priceResult.setAssetInfo(ul, 1.0 / k)
      }
    }
  }

  override def toString = s"${leverage.asPercent} x Min{[${strikes}]}"
  
  override def priceImpl(priceResult:PriceResult) = {
    if (isFixed) {
      if (physical) {
        assignPhysicalInfo(priceResult)
        Double.NaN
      } else Double.NaN
    }
    else Double.NaN
  }
  
  override def jsonMapImpl = Map(
    "type" -> "forward", 
    "variable" -> strikes.keySet,
    "strike" -> strikes.getDouble.map{case (ul, v) => (ul, v)},
    "description" -> description
  )

  override def fixedConditions:Map[String, Any] = {
    Map(
      "strike" -> strikes.getDouble.map{case (ul, v) => (ul, v)},
    )
  }


}

object ForwardPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):ForwardPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)
    val fixedNode = fixed.jsonNode

    val variables:List[String] = formula.parseJsonStringList("variable").map(_.orNull)

    val strikes:UnderlyingFixing = UnderlyingFixing(fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "strike", variables)}.getOrElse(Map.empty))(fixingInfo.getInitialFixingInformation)

    val physical:Boolean = formula.parseJsonString("physical").getOrElse("0") == "1"
//    val reverse:Boolean = formula.parseJsonString("reverse").getOrElse("0") == "1"
    val minPayoff:Double = fixed.parseJsonDouble("min").getOrElse(0.0)
    val maxPayoff:Option[Double] = fixed.parseJsonDouble("max")
    val leverage:Double = fixed.parseJsonDouble("leverage").getOrElse(1.0)
    val description:String = fixingInfo.update(formula).parseJsonString("description").orNull

    ForwardPayoff(
      strikes = strikes,
      physical = physical,
      leverage = leverage,
      minPayoff = minPayoff,
      maxPayoff = maxPayoff,
      description = description,
      inputString = inputString
    )
  }


}

