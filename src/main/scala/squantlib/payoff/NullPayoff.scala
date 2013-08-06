package squantlib.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._

/**
 * Interprets JSON formula specification for a fixed leg.
 * JSON format: {type:"fixed", description:XXX, payoff:double}
 * Natual format: 0.035 or "3.5%"
 */
case class NullPayoff(
    description:String = null, 
    inputString:String = null) extends Payoff {
	
	override val variables:Set[String] = Set.empty
	
	override val isPriceable = false
	
	override val isFixed = false
	 
	override def priceImpl(fixings:Map[String, Double]) = Double.NaN
	
	override def priceImpl(fixing:Double) = Double.NaN
	
	override def priceImpl = Double.NaN
	
	override def toString = description
	
	override def jsonString = inputString
	
}


object NullPayoff {
  
	def apply(formula:String):NullPayoff = {
	  val description:String = formula.parseJsonString("description").orNull
	  NullPayoff(description, formula)
	}
}

