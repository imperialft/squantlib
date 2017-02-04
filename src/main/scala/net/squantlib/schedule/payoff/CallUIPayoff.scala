package net.squantlib.schedule.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.map.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import java.util.{Map => JavaMap}
import net.squantlib.util.FixingInformation

/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 * {"type":"callui","variable":[String],"trigger":Double, "strike":[Double], "add":Double, "mult": Double, "min":Double, "max":Double, "baseAmount":Double, "basket":String, "description":""}
 */
case class CallUIPayoff(
    callVariables:List[String], 
    trigger:Double, 
    strike:List[Double], 
    mult: Double,
    added: Double,
    maxPayoff : Option[Double],
    minPayoff : Option[Double],
    basket: String,
    baseAmount: Double = 1.0,
    amount:Double = 1.0,
    description:String = null,
    inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {
  
  val variables = callVariables.toSet
  
  nominal = amount
  
  override val isPriceable:Boolean = !trigger.isNaN && !trigger.isInfinity && strike.forall(s => !s.isNaN && !s.isInfinity)
  
  def getFixings(fixings:Map[String, Double]):Option[List[Double]] = 
    if (variables.toSet subsetOf fixings.keySet) 
      Some((0 to callVariables.size - 1).toList.map(i => fixings(callVariables(i))))
    else None
    
  def basketPerformance(fixings:Map[String, Double]):Option[Double] = 
    getFixings(fixings) match {
      case Some(fixValues) if fixValues.forall(v => !v.isNaN && !v.isInfinity) => 
      	basket match {
      	  case "average" => Some((fixValues, strike).zipped.map((v, k) => v/k).sum / fixValues.size.toDouble)
      	  case "max" => Some((fixValues, strike).zipped.map((v, k) => v/k).max)
      	  case _ => Some((fixValues, strike).zipped.map((v, k) => v/k).min)
      	}
      case _ => None
    }
      
  override def priceImpl(fixings:Map[String, Double]) = 
    basketPerformance(fixings) match {
      case Some(v) if !v.isNaN && !v.isInfinity  => 
        if (v <= trigger) baseAmount
        else {
          val perf = 1.0 + (v - 1.0) * mult + added
          (maxPayoff, minPayoff) match {
            case (Some(max), Some(min)) => math.max(min, math.min(max, perf))
            case (Some(max), None) => math.min(max, perf)
            case (None, Some(min)) => math.max(min, perf)
            case _ => perf
          }
        }
      case _ => Double.NaN
    }
    
  override def priceImpl(fixing:Double) =
    if (variables.size != 1 || fixing.isNaN || fixing.isInfinity) Double.NaN
    else {
      val ulperf = fixing / strike.head
      if (ulperf <= trigger) baseAmount
      else {
        val perf = 1.0 + (ulperf - 1.0) * mult + added
        (maxPayoff, minPayoff) match {
          case (Some(max), Some(min)) => math.max(min, math.min(max, perf))
          case (Some(max), None) => math.min(max, perf)
          case (None, Some(min)) => math.max(min, perf)
          case _ => perf
        }
      }
    }
   
  override def toString =
    baseAmount.asPercent + " [" + trigger.asPercent + "] " + nominal.asPercent + " + " + mult.asPercent + " x " + basket + "([" + variables.mkString(",") + "] / [" + strike.mkString(",") + "]) + " + added.asPercent
  
  override def priceImpl = Double.NaN
  
  override def jsonMapImpl = Map(
    "type" -> "callui", 
    "variable" -> callVariables.toArray, 
    "strike" -> strike.toArray, 
    "trigger" -> trigger, 
    "added" -> added,
    "amount" -> amount,
    "mult" -> mult,
    "max" -> maxPayoff,
    "min" -> minPayoff,
    "basket" -> basket,
    "description" -> description)

  
}

object CallUIPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):CallUIPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)

    CallUIPayoff(
      callVariables = formula.parseJsonStringList("variable").map(_.orNull), 
      trigger = fixed.parseJsonDouble("trigger").getOrElse(Double.NaN), 
      strike = fixed.parseJsonDoubleList("strike").map(_.getOrElse(Double.NaN)), 
      mult = fixed.parseJsonDouble("mult").getOrElse(1.0),
      added = fixed.parseJsonDouble("add").getOrElse(0.0),
      maxPayoff = fixed.parseJsonDouble("max"),
      minPayoff = fixed.parseJsonDouble("min"),
      basket = formula.parseJsonString("basket").getOrElse("worst"),
      baseAmount = fixed.parseJsonDouble("baseAmount").getOrElse(1.0),
      amount = fixed.parseJsonDouble("amount").getOrElse(1.0),
      description = formula.parseJsonString("description").orNull,
      inputString = inputString
    )
    
  }
  
}

