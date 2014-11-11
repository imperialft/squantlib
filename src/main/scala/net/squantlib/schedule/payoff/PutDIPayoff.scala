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
 *  {type:"putdi", variable:[String], trigger:[Double], strike:[Double], description:String}, 
 * No strike is considered as no low boundary
 */
case class PutDIPayoff(
    putVariables:List[String], 
    trigger:List[Double], 
    strike:List[Double], 
    amount:Double = 1.0,
    description:String = null,
    inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {
  
  val variables = putVariables.toSet
  
  nominal = amount
  
  override val isPriceable:Boolean = !trigger.exists(v => v.isNaN || v.isInfinity) && !strike.exists(v => v.isNaN || v.isInfinity)
  
  def getFixings(fixings:Map[String, Double]):Option[List[Double]] = 
    if (variables.toSet subsetOf fixings.keySet) 
      Some((0 to putVariables.size - 1).toList.map(i => fixings(putVariables(i))))
    else None
      
  override def priceImpl(fixings:Map[String, Double]) = 
    getFixings(fixings) match {
      case Some(fixValues) if fixValues.forall(v => !v.isNaN && !v.isInfinity) => 
        if (fixValues.corresponds(trigger) {_ >= _}) 1.0
        else math.min(1.00, (fixValues, strike).zipped.map((v, k) => v/k).min)
      case None => Double.NaN
    }
    
  override def priceImpl(fixing:Double) =
    if (variables.size != 1 || fixing.isNaN || fixing.isInfinity) Double.NaN
    else if (fixing >= trigger.head) 1.0
    else math.min(1.00, fixing / strike.head)
   
  override def toString =
    nominal.asPercent + " [" + trigger.mkString(",") + "] " + nominal.asPercent + " x Min([" + variables.mkString(",") + "] / [" + strike.mkString(",") + "])"
  
  override def priceImpl = Double.NaN
  
  override def jsonMapImpl = Map(
    "type" -> "putdi", 
    "variable" -> putVariables.toArray, 
    "trigger" -> trigger.toArray, 
    "strike" -> strike.toArray, 
    "description" -> description)

  
}

object PutDIPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):PutDIPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)
    val variable:List[String] = formula.parseJsonStringList("variable").map(_.orNull)  
    val trigger:List[Double] = fixed.parseJsonDoubleList("trigger").map(_.getOrElse(Double.NaN))
    val strike:List[Double] = fixed.parseJsonDoubleList("strike").map(_.getOrElse(Double.NaN))
    val amount:Double = fixed.parseJsonDouble("amount").getOrElse(1.0)
    val description:String = formula.parseJsonString("description").orNull
    PutDIPayoff(variable, trigger, strike, amount, description, inputString)
  }
  
}

