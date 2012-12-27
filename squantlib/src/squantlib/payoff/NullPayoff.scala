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
case class NullPayoff(description:String = null, inputString:String = null) extends Payoff {
	
	val variables:Set[String] = Set.empty
	 
	override def price(fixings:Map[String, Double]) = Double.NaN
	override def price(fixing:Double)(implicit d:DummyImplicit) = Double.NaN
	override def price = Double.NaN
	
	override def toString = description
	
	override val jsonString = inputString
	
}


object NullPayoff {
  
	def apply(formula:String):NullPayoff = {
	  val description:String = formula.parseJsonString("description")
	  NullPayoff(description, formula)
	}
}

