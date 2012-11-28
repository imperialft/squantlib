package squantlib.payoff

import scala.collection.JavaConversions._
import squantlib.payoff.DisplayUtils._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper

import JsonUtils._

/**
 * Interprets JSON formula specification for a linear formula with cap & floor.
 * JSON format:
 * - {type:"linear1d", variable:string, payoff:formula}, where
 *   formula = {min:double, max:double, mult:double, add:double, description:XXX}
 *   payment for array(i) is min <= mult * variable + add <= max
 */
case class Linear1dPayoff(val variable:String, val payoff:Linear1dFormula) extends Payoff {
  
	val variables:Set[String] = if (variable == null) Set.empty else Set(variable)
	 
	override def price(fixings:Map[String, Double]) = 
	  if (fixings contains variable) Some(payoff.price(fixings(variable)))
	  else None
	
	override def price(fixing:Double)(implicit d:DummyImplicit) = Some(payoff.price(fixing))
	
	override def price = None
	
	override def toString:String = payoff.toString(variable)
	
}

object Linear1dPayoff {
  
	def apply(formula:String):Linear1dPayoff = {
	  val variable:String = formula.parseJsonString("variable")	
	  
	  val payoff:Linear1dFormula = formula.jsonnode("payoff") match {
		  case Some(n) => Linear1dFormula(n)
		  case None => null
		}
	  
	  Linear1dPayoff(variable, payoff)
	}
	
	def apply(variable:String, payoff:JsonNode):Linear1dPayoff = Linear1dPayoff(variable, Linear1dFormula(payoff))
	
	def apply(variable:String, coeff:Option[Double], constant:Option[Double], minValue:Option[Double], maxValue:Option[Double], description:String = null):Linear1dPayoff = 
	  Linear1dPayoff(variable, Linear1dFormula(coeff, constant, minValue, maxValue, description))
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
