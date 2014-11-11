package net.squantlib.schedule.payoff

import scala.collection.JavaConversions._
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.util.FixingInformation

/**
 * Interprets JSON formula specification for a fixed leg.
 * JSON format: {type:"fixed", description:XXX, payoff:double}
 * Natual format: 0.035 or "3.5%"
 */
case class NullPayoff(
    description:String = null, 
    inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {
  
  override val variables:Set[String] = Set.empty
  
  override val isPriceable = false
  
  override val isFixed = false
   
  override def priceImpl(fixings:Map[String, Double]) = Double.NaN
  
  override def priceImpl(fixing:Double) = Double.NaN
  
  override def priceImpl = Double.NaN
  
  override def toString = description
  
  override def jsonMapImpl = Map.empty
  
}


object NullPayoff {
  
  def apply(formula:String)(implicit fixingInfo:FixingInformation):NullPayoff = {
    val description:String = fixingInfo.update(formula).parseJsonString("description").orNull
    NullPayoff(description, formula)
  }
}

