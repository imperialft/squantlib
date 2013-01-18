package squantlib.payoff

import scala.collection.JavaConversions._
import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._

/**
 * Interprets JSON formula for series of linear formulas with caps and floors.
 * JSON format:
 * - {type:"fixedseries", description:XXX, payoff:Array[double]}
 */
object FixedPayoffSeries {
  
	def apply(formula:String):List[FixedPayoff] = formula.jsonNode match {
	    case Some(n) if n isObject => n.get("payoff").getElements.map(e => FixedPayoff(e.parseJsonDouble.getOrElse(Double.NaN))).toList
		case Some(n) if n isArray => n.getElements.map(e => FixedPayoff(e.parseJsonDouble.getOrElse(Double.NaN))).toList
		case Some(n) if n isNumber => List(FixedPayoff(n.parseJsonDouble.getOrElse(Double.NaN)))
		case _ => List.empty
	  }
	
}

