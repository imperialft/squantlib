package squantlib.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._

/**
 * Interprets JSON formula for series of linear formulas with caps and floors.
 * JSON format:
 * - {type:"linear1dseries", variable:string, payoff:Array[formula]}, where
 *   formula = {min:double, max:double, mult:double, add:double, description:String}
 *   payment for array(i) is min <= mult * variable + add <= max
 */
object Linear1dPayoffSeries {
  
	def apply(formula:String):List[Linear1dPayoff] = {
	  val variable = formula.parseJsonString("variable").orNull
	  
	  formula.jsonArray("payoff").map(_.parseObject(Linear1dPayoff(variable, _))).flatMap(s => s)
	  
//	  formula.jsonNode("payoff") match {
//	    case Some(n) if n.isArray => n.getElements.map(Linear1dPayoff(variable, _)).toList
//		case Some(n) if n.isObject => List(Linear1dPayoff(variable, n))
//		case _ => List.empty
//	  }
	}
}