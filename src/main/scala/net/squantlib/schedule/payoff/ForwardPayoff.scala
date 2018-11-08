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
  strikes:Map[String, Double],
  override val physical:Boolean,
  reverse:Boolean,
  override val minPayoff:Double,
  override val maxPayoff:Option[Double],
  description:String = null,
  inputString:String = null
)(implicit val fixingInfo:FixingInformation) extends Payoff {
  
  override val variables = strikes.keySet
  
  override val isPriceable = !strikes.exists{case (k, v) => v.isNaN || v.isInfinity} && !strikes.isEmpty

  override def eventDates(period:CalculationPeriod):List[Date] = {
    if (physical) List(period.paymentDate)
    else List(period.eventDate)
  }
  
//  def parseFixings(fixings:Map[String, Double]):Option[List[Double]] = {
//    if (variables subsetOf fixings.keySet)
//      Some((0 to fwdVariables.size - 1).toList.map(i => fixings(fwdVariables(i))))
//    else None
//  }

  def getPerformance(p:Double, stk:Double):Double = {
    if (reverse) withMinMax(2.0 - p / stk)
    else withMinMax(p / stk)
  }

  override def priceImpl(fixings:List[Map[String, Double]], pastPayments:List[Double], priceResult:PriceResult) =
    fixings.lastOption.collect{case f => priceImpl(f, pastPayments, priceResult)}.getOrElse(Double.NaN)

  def priceImpl(fixings:Map[String, Double], pastPayments:List[Double], priceResult:PriceResult) = {
    if ((variables subsetOf fixings.keySet) && variables.forall(ul => !fixings(ul).isNaN && !fixings(ul).isInfinity) && isPriceable) {
      if (physical && priceResult != null) {
        variables.map(ul => (ul, getPerformance(fixings(ul), strikes(ul)), strikes(ul))).minBy{case (ul, perf, k) => perf} match {
          case (ul, pf, k) =>
            priceResult.setAssetInfo(ul, 1.0 / k)
            withMinMax(pf)
        }
      } else {
        withMinMax(variables.map(ul => getPerformance(fixings(ul), strikes(ul))).min)
      }
    } else Double.NaN


    //    parseFixings(fixings) match {
//      case Some(fixValues) if fixValues.forall(v => !v.isNaN && !v.isInfinity) =>
//        val perfs = (fixValues, strike).zipped.map((v, k) => v / k)
//        if (physical && priceResult != null) {
//          (perfs, strike, fwdVariables).zipped.minBy{case (pf, k, ul) => pf} match {
//            case (pf, k, ul) => priceResult.setAssetInfo(ul, 1.0 / k)
//          }
//        }
//        perfs.min
//      case _ => Double.NaN
//    }
  }

//  override def priceImpl(fixing:Double, pastPayments:List[Double]) =
//    if (variables.size != 1 || fixing.isNaN || fixing.isInfinity) Double.NaN
//    else fixing / strike.head
  
  override def toString =
    "Min{[" + strikes.map{case (k, v) => s"${k}: ${v.asDouble}"}.mkString(",") + "]}"
  
  override def priceImpl(priceResult:PriceResult) = {
    Double.NaN
  }
  
  override def jsonMapImpl = Map(
    "type" -> "forward", 
    "variable" -> strikes.keySet,
    "strike" -> strikes, //(fwdVariables, strike).zipped.toMap,
    "description" -> description
  )

	
}

object ForwardPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):ForwardPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)
    val fixedNode = fixed.jsonNode

    val variables:List[String] = formula.parseJsonStringList("variable").map(_.orNull)

    val strikes:Map[String, Double] = fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "strike", variables)}.getOrElse(Map.empty)

//    val variable:List[String] = fixingInfo.update(formula).parseJsonStringList("variable").map(_.orNull)
//    val strike:List[Double] = fixingInfo.update(formula).parseJsonDoubleList("strike").map(_.getOrElse(Double.NaN))
    val physical:Boolean = formula.parseJsonString("physical").getOrElse("0") == "1"
    val reverse:Boolean = formula.parseJsonString("reverse").getOrElse("0") == "1"
    val minPayoff:Double = fixed.parseJsonDouble("min").getOrElse(0.0)
    val maxPayoff:Option[Double] = fixed.parseJsonDouble("max")
    val description:String = fixingInfo.update(formula).parseJsonString("description").orNull

    ForwardPayoff(
      strikes = strikes,
      physical = physical,
      reverse = reverse,
      minPayoff = minPayoff,
      maxPayoff = maxPayoff,
      description = description,
      inputString = inputString
    )
  }

}

