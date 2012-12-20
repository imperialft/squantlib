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
case class FixedPayoff(payoff:Double, description:String = null) extends Payoff {
	
	val variables:Set[String] = Set.empty
	 
	override def price(fixings:Map[String, Double]) = payoff
	override def price(fixing:Double)(implicit d:DummyImplicit) = payoff
	override def price = payoff
	
	override def toString = payoff.asPercent
	
	override val jsonString = {
	  val infoMap:java.util.Map[String, Any] = Map("type" -> "fixed", "description" -> description, "payoff" -> payoff)
	  (new ObjectMapper).writeValueAsString(infoMap)	  
	}
	
}


object FixedPayoff {
  
	def apply(formula:String):FixedPayoff = formula.parseDouble match {
	  case Some(v) => FixedPayoff(v)
	  case None => FixedPayoff(formula.parseJsonDouble("payoff").getOrElse(Double.NaN), formula.parseJsonString("description"))
	}
	
	def apply(payoff:Double):FixedPayoff = new FixedPayoff(payoff)

}