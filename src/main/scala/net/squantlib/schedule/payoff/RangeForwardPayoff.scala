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
 *  {type:"rangeforward", variable:String, triggerhigh:Double, triggerlow:Double, strike:Double, description:String}, 
 * No strike is considered as no low boundary
 */
case class RangeForwardPayoff(
    variable:String, 
    triggerLow:Double, 
    triggerHigh:Double, 
    strike:Double, 
    amount:Double = 1.0, 
    description:String = null,
    inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {
  
  val variables = Set(variable)
  
  override val isPriceable:Boolean = 
    !triggerHigh.isNaN && !triggerHigh.isInfinity && 
    !triggerLow.isNaN && !triggerLow.isInfinity && 
    !strike.isNaN && !strike.isInfinity
  
//  def getFixings(fixings:Map[String, Double]):Option[List[Double]] = 
//    if (variables.toSet subsetOf fixings.keySet) 
//      Some((0 to putVariables.size - 1).toList.map(i => fixings(putVariables(i))))
//    else None
      
  override def priceImpl(fixings:Map[String, Double]) = 
    fixings.get(variable) match {
      case Some(v) if !v.isNaN && !v.isInfinity => priceImpl(v)
      case None => Double.NaN
    }
    
  override def priceImpl(fixing:Double):Double =
    if (fixing < triggerLow || fixing > triggerHigh) amount
    else amount * fixing / strike
   
  override def toString =
    amount.asPercent + " [" + triggerHigh.asDouble + ", " + triggerLow.asDouble + "] " + amount.asPercent + " x " + variable + " / " + strike.asDouble + ""
  
  override def priceImpl = Double.NaN
  
  override def jsonString = {
    
    val infoMap:JavaMap[String, Any] = Map(
        "type" -> "putdi", 
        "variable" -> variable, 
        "triggerhigh" -> triggerHigh, 
        "triggerlow" -> triggerLow, 
        "strike" -> strike, 
        "description" -> description)
    
    (new ObjectMapper).writeValueAsString(infoMap)    
  }  
  
}

object RangeForwardPayoff {
  
  def apply(formula:String)(implicit fixingInfo:FixingInformation):RangeForwardPayoff = {
    val fixed = fixingInfo.update(formula)
    val variable:String = formula.parseJsonString("variable").getOrElse(null)
    val triggerHigh:Double = fixed.parseJsonDouble("triggerhigh").getOrElse(Double.NaN)
    val triggerLow:Double = fixed.parseJsonDouble("triggerlow").getOrElse(Double.NaN)
    val strike:Double = fixed.parseJsonDouble("strike").getOrElse(Double.NaN)
    val amount:Double = fixed.parseJsonDouble("amount").getOrElse(1.0)
    val description:String = formula.parseJsonString("description").orNull
    RangeForwardPayoff(variable, triggerHigh, triggerLow, strike, amount, description, formula)
  }
  
}

