package squantlib.payoff

import scala.collection.JavaConversions._
import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper

/**
 * Interprets JSON formula specification for a linear formula with cap & floor.
 * JSON format:
 * - {type:"linear1d", variable:string, payoff:formula}, where
 *   formula = {min:double, max:double, mult:double, add:double, description:XXX}
 *   payment for array(i) is min <= mult * variable + add <= max
 */
case class Linear1dPayoff(variable:String, payoff:Linear1dFormula, description:String) extends Payoff {
  
	val variables:Set[String] = if (variable == null) Set.empty else Set(variable)
	 
	override def price(fixings:Map[String, Double]) = 
	  if (fixings contains variable) payoff.price(fixings(variable))
	  else Double.NaN
	
	override def price(fixing:Double)(implicit d:DummyImplicit) = payoff.price(fixing)
	
	override def price = Double.NaN
	
	override def toString:String = payoff.toString(variable)
	
}

object Linear1dPayoff {
  
	def apply(formula:String):Linear1dPayoff = {
	  val variable:String = formula.parseJsonString("variable")	
	  
	  val payoff:Linear1dFormula = formula.jsonNode("payoff") match {
		  case Some(n) => Linear1dFormula(n)
		  case None => null
		}
	  
	  Linear1dPayoff(variable, payoff, null)
	}
	
	def apply(variable:String, payoff:JsonNode):Linear1dPayoff = Linear1dPayoff(variable, Linear1dFormula(payoff), null)
	
	def apply(variable:String, coeff:Option[Double], constant:Option[Double], minValue:Option[Double], maxValue:Option[Double], description:String = null):Linear1dPayoff = 
	  Linear1dPayoff(variable, Linear1dFormula(coeff, constant, minValue, maxValue, description), description)
}

case class Linear1dFormula (val coeff:Option[Double], val constant:Option[Double], val minValue:Option[Double], val maxValue:Option[Double], val description:String) {

	def price(fixing:Double):Double = {
	  var p = (coeff, constant) match {
	    case (None, None) => 0.0
		case (None, Some(c)) => c
		case (Some(x), None) => x * fixing
		case (Some(x), Some(c)) => x * fixing + c
	  }
	  
	  if (minValue.isDefined) p = p.max(minValue.get)
	  if (maxValue.isDefined) p = p.min(maxValue.get)
	  p
	} 
	
	def toString(varname:String) = description textOr 
									(coeff.asDoubleOr("0") + " * " + varname + " + " + constant.asPercentOr("0") + 
									minValue.asPercentOr("", " min ", "") + maxValue.asPercentOr("", " max ", ""))
}

object Linear1dFormula {
  
	def apply(subnode:JsonNode):Linear1dFormula = {
		val minValue:Option[Double] = subnode.parseJsonDouble("min")
		val maxValue:Option[Double] = subnode.parseJsonDouble("max")
		val coeff:Option[Double] = subnode.parseJsonDouble("mult")
		val constant:Option[Double] = subnode.parseJsonDouble("add")
		val description:String = subnode.parseJsonString("description")
		Linear1dFormula(coeff, constant, minValue, maxValue, description)
	}
	
}
