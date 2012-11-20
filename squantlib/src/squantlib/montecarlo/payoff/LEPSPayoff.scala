package squantlib.montecarlo.payoff

import squantlib.setting.initializer.Currencies
import scala.collection.mutable.{Map => mutableMap}
import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper

/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 * - {variable:string, formula}, where
 *   formula = array {minrange:double, maxrange:double, coeff:double, constant:double}
 *   payment for array(i) is 
 *     if minrange(i) <= X < maxrange(i) => coeffs(i) * variable + constant(i)
 *     otherwise zero
 */
class LEPS1dPayoff(val formula:String) extends Payoff {
  
	val mapper = new ObjectMapper
	val node = mapper.readTree(formula)
	val LEPSformula = LEPS1dFormula(node.get("formula"))
  
	val variable:String = node.get("variable").getTextValue
	val variables:Set[String] = Set(variable)
	 
	def price(fixings:Map[String, Double]):Option[Double] = 
	  if (fixings.contains(variable)) price(fixings(variable))
	  else None
	
	override def price(fixing:Double):Option[Double] = Some(LEPSformula.price(fixing))
	
}

case class LEPS1dFormula (val node:JsonNode) {
  
	val formula:Array[LEPS1dComponent] = node.getElements.map(LEPS1dComponent).toArray
	
	def price(fixing:Double):Double = formula.map(_.price(fixing)).sum
}

case class LEPS1dComponent (val subnode:JsonNode) {
	
	val fields:Set[String] = subnode.getFieldNames.toSet
	
	private def getvalue(name:String):Option[Double] = 
	  if (fields.contains(name)) Some(subnode.get(name).getDoubleValue)
	  else None
	
	val minRange:Option[Double] = getvalue("minrange")
	val maxRange:Option[Double] = getvalue("maxrange")
	val coeff:Option[Double] = getvalue("coeff")
	val constant:Option[Double] = getvalue("constant")
	 
	def price(fixing:Double):Double = {
	  minRange match {
	    case Some(f) if fixing < f => return 0.0
	    case _ =>
	  }
	  
	  maxRange match {
	    case Some(c) if fixing >= c => return 0.0
	    case _ =>
	  }
	   
	  (coeff, constant) match {
	    case (None, None) => 0.0
		case (None, Some(c)) => c
		case (Some(x), None) => x * fixing
		case (Some(x), Some(c)) => x * fixing + c
	  }
	}
	
}


