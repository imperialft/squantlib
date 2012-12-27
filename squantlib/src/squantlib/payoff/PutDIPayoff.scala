package squantlib.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import squantlib.util.FormulaParser
import java.util.{Map => JavaMap}


/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 * - {type:"putdi", variable:string, amount:Double, trigger:Double, strike:Double, description:String}, where
 *   formula =  if x <= trigger => amount * x / strike
 *   			if x > trigger => amount
 */
case class PutDIPayoff(varname:String, trigger:Double, strike:Double, desc:String = null) 
	extends LEPS1dPayoff(varname, 
	    List(LEPS1dComponent(Some(1.0 / strike), None, None, Some(trigger)),
	    LEPS1dComponent(None, Some(1.0), Some(trigger), None)),
	    desc) {
  
	
	override def toString = "[," + trigger.asDouble + "] " + varname + "/" + strike.asDouble + " [" + trigger.asDouble + ",] " + "100%"
	
	override val jsonString = {
	  val infoMap:JavaMap[String, Any] = Map(
	      "type" -> "putdi", 
	      "variable" -> variable, 
	      "trigger" -> trigger,
	      "strike" -> strike,
	      "description" -> desc)
	      
	  (new ObjectMapper).writeValueAsString(infoMap)	  
	}	
	
}

object PutDIPayoff {
	
	def apply(node:String):PutDIPayoff = {
	  val variable:String = node.parseJsonString("variable")
	  val trigger:Double = node.parseJsonDouble("trigger").getOrElse(Double.NaN)
	  val strike:Double = node.parseJsonDouble("strike").getOrElse(Double.NaN)
	  val description:String = node.parseJsonString("description")
	  PutDIPayoff(variable, trigger, strike, description)
	}
  
}