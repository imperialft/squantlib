package net.squantlib.schedule.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.map.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.util.FixingInformation
import java.util.{Map => JavaMap}


/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 *  {type:"forward", variable:[String], trigger:[Double], strike:[Double], description:String}, 
 * No strike is considered as no low boundary
 */
case class ForwardPayoff(
    fwdVariables:List[String], 
    strike:List[Double], 
    description:String = null,
    inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {
  
  override val variables = fwdVariables.toSet
  
  override val isPriceable = !strike.exists(v => v.isNaN || v.isInfinity) && !strike.isEmpty && !fwdVariables.isEmpty
  
  def getFixings(fixings:Map[String, Double]):Option[List[Double]] = 
    if (variables.toSet subsetOf fixings.keySet) 
      Some((0 to fwdVariables.size - 1).toList.map(i => fixings(fwdVariables(i))))
    else None
      
  override def priceImpl(fixings:Map[String, Double]) = 
    getFixings(fixings) match {
      case Some(fixValues) if fixValues.forall(v => !v.isNaN && !v.isInfinity) => (fixValues, strike).zipped.map((v, k) => v/k).min
      case None => Double.NaN
    }
    
  override def priceImpl(fixing:Double) =
    if (variables.size != 1 || fixing.isNaN || fixing.isInfinity) Double.NaN
    else fixing / strike.head
  
  override def toString =
    "Min{[" + variables.mkString(",") + "] / [" + strike.mkString(",") + "]}"
  
  override def priceImpl = Double.NaN
  
  override def jsonMapImpl = Map(
    "type" -> "forward", 
    "variable" -> fwdVariables.toArray, 
    "strike" -> strike.toArray, 
    "description" -> description)

	
}

object ForwardPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):ForwardPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val variable:List[String] = fixingInfo.update(formula).parseJsonStringList("variable").map(_.orNull)
    val strike:List[Double] = fixingInfo.update(formula).parseJsonDoubleList("strike").map(_.getOrElse(Double.NaN))
    val description:String = fixingInfo.update(formula).parseJsonString("description").orNull
    ForwardPayoff(variable, strike, description, inputString)
  }
  
}

