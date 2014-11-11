package net.squantlib.schedule.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.map.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.util.FixingInformation

/**
 * Interprets JSON formula specification for a fixed leg.
 * JSON format: {type:"fixed", description:XXX, payoff:double}
 * Natual format: 0.035 or "3.5%"
 */
case class FixedPayoff(
    payoff:Double, 
    description:String = null,
    inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {
	
  override val variables:Set[String] = Set.empty
  
  override val isPriceable = !payoff.isNaN && !payoff.isInfinity
  
  override val isFixed = true
   
  override def priceImpl(fixings:Map[String, Double]) = payoff
  
  override def priceImpl(fixing:Double) = payoff
  
  override def priceImpl = payoff
  
  override def toString = payoff.asPercent
  
  override def jsonMapImpl = {
    Map("type" -> "fixed", "description" -> description, "payoff" -> payoff)
  }
	
}


object FixedPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):FixedPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    fixingInfo.update(formula).parseDouble match {
      case Some(v) => FixedPayoff(v, null, inputString)
      case None => FixedPayoff(fixingInfo.update(formula).parseJsonDouble("payoff").getOrElse(Double.NaN), formula.parseJsonString("description").orNull, inputString)
    }
  }
	
  def apply(payoff:Double)(implicit fixingInfo:FixingInformation):FixedPayoff = new FixedPayoff(payoff, null, null)

}