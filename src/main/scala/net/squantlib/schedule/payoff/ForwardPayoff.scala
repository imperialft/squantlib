package net.squantlib.schedule.payoff

import scala.collection.JavaConversions._
import com.fasterxml.jackson.databind.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.util.FixingInformation
import java.util.{Map => JavaMap}
import net.squantlib.util.Date
import net.squantlib.schedule.CalculationPeriod
import scala.reflect.ClassTag


/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 *  {type:"forward", variable:[String], trigger:[Double], strike:[Double], description:String}, 
 * No strike is considered as no low boundary
 */
case class ForwardPayoff(
  strikeDefinition:Map[String, Option[BigDecimal]],
  override val physical:Boolean,
  reverse:Boolean,
  override val minPayoff:Double,
  override val maxPayoff:Option[Double],
  description:String = null,
  inputString:String = null
)(implicit val fixingInfo:FixingInformation) extends Payoff {

  val strikes = strikeDefinition.collect{case (ul, Some(v)) => (ul, v)}

  override val variables = strikeDefinition.keySet
  
  override val isPriceable = !strikes.isEmpty && strikeDefinition.values.forall(_.isDefined)

  override def eventDates(period:CalculationPeriod):List[Date] = {
    if (physical) List(period.eventDate, period.paymentDate)
    else List(period.eventDate)
  }
  
  def getPerformance(p:Double, stk:Double):Double = {
    if (reverse) withMinMax(2.0 - p / stk)
    else withMinMax(p / stk)
  }

  override def priceImpl(fixings:List[Map[String, Double]], pastPayments:List[Double], priceResult:PriceResult) = {
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

  def priceImpl(fixings:Map[String, Double], pastPayments:List[Double], priceResult:PriceResult) = {
    if ((variables subsetOf fixings.keySet) && variables.forall(ul => !fixings(ul).isNaN && !fixings(ul).isInfinity) && isPriceable) {
      if (physical) {
        if (isFixed) assignPhysicalInfo(priceResult)
        variables.map(ul => (ul, getPerformance(fixings(ul), strikes(ul).toDouble), strikes(ul))).minBy{case (ul, perf, k) => perf} match {
          case (ul, pf, k) =>
            withMinMax(pf)
        }
      } else {
        withMinMax(variables.map(ul => getPerformance(fixings(ul), strikes(ul).toDouble)).min)
      }
    } else Double.NaN

  }

  def assignPhysicalInfo(priceResult:PriceResult):Unit = {
    if (isFixed) assignPhysicalInfo(getDoubleFixings, priceResult)
  }

  def assignPhysicalInfo(fixings:Map[String, Double], priceResult:PriceResult):Unit = {
    if (priceResult != null && variables.subsetOf(fixings.keySet)) {
      variables.map(ul => (ul, getPerformance(fixings(ul), strikes(ul).toDouble), strikes(ul))).minBy{case (ul, perf, k) => perf} match {
        case (ul, pf, k) => priceResult.setAssetInfo(ul, 1.0 / k.toDouble)
      }
    }
  }

  override def toString =
    "Min{[" + strikes.map{case (k, v) => s"${k}: ${v.asDouble}"}.mkString(",") + "]}"
  
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
    "variable" -> strikeDefinition.keySet,
    "strike" -> strikeDefinition.map{case (ul, v) => (ul, v.collect{case vv => vv.toDouble}.getOrElse(Double.NaN))},
    "description" -> description
  )

	
}

object ForwardPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):ForwardPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)
    val fixedNode = fixed.jsonNode

    val variables:List[String] = formula.parseJsonStringList("variable").map(_.orNull)

    val strikes:Map[String, Option[BigDecimal]] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "strike", variables).getOptionalDecimal}.getOrElse(Map.empty)

    val physical:Boolean = formula.parseJsonString("physical").getOrElse("0") == "1"
    val reverse:Boolean = formula.parseJsonString("reverse").getOrElse("0") == "1"
    val minPayoff:Double = fixed.parseJsonDouble("min").getOrElse(0.0)
    val maxPayoff:Option[Double] = fixed.parseJsonDouble("max")
    val description:String = fixingInfo.update(formula).parseJsonString("description").orNull

    ForwardPayoff(
      strikeDefinition = strikes,
      physical = physical,
      reverse = reverse,
      minPayoff = minPayoff,
      maxPayoff = maxPayoff,
      description = description,
      inputString = inputString
    )
  }


}

