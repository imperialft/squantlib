package squantlib.montecarlo.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import DisplayUtils._
import JsonUtils._

/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 * - {type:"leps1d", variable:string, description:String, payoff:formula}, where
 *   formula = Array {minrange:double, maxrange:double, mult:double, add:double}
 *   payment for array(i) is 
 *     if minrange(i) <= X < maxrange(i) => mult(i) * variable + add(i)
 *     otherwise zero
 */
case class LEPS1dPayoff(val variable:String, val payoff:List[LEPS1dComponent], val description:String = null) extends Payoff {
  
	override val variables:Set[String] = Set(variable)
	  
	override def price(fixings:Map[String, Double]) = 
	  if (fixings contains variable) price(fixings(variable))
	  else None
	
	override def price(fixing:Double)(implicit d:DummyImplicit) = Some(payoff.map(_.price(fixing)).sum)
	override def toString = description textOr payoff.map(_.toString(variable)).mkString(" ")
	
	override def price = None
	
}


object LEPS1dPayoff {
  
	def apply(inputformula:String):LEPS1dPayoff = {
	  val formula = inputformula.trim
	  
	  if (formula.startsWith("leps")) {
	    val formulalist = FormulaParser.parseList(formula.substring(4))
	    val variables = formulalist.map{case (f, _, _) => f.keySet}.flatten.flatten.toSet
	    assert(variables.size <= 1)
	    
	    val variable = Set(variables.head)
	    val components = formulalist.map{case (formula, minrange, maxrange) => {
	      val coeff = formula.get(variable)
	      val const = formula.get(Set.empty)
	      LEPS1dComponent(coeff, const, minrange, maxrange)
	    }}
	    LEPS1dPayoff(variables.head, components)
	  }
	  
	  else {
	    val variable:String = formula.parseJsonString("variable")
	    val description:String = formula.parseJsonString("description")
	  
	    val payoff:List[LEPS1dComponent] = formula.jsonnode match {
	      case Some(node) => getLEPScomponents(node.get("payoff"))
	      case None => List.empty
	    }
	    
	    LEPS1dPayoff(variable, payoff, description)
	  }
	}

	def apply(variable:String, payoff:JsonNode, description:String):LEPS1dPayoff = 
	  LEPS1dPayoff(variable, getLEPScomponents(payoff), description)
	  
	def getLEPScomponents(node:JsonNode):List[LEPS1dComponent] = node match {
	  case null => List.empty
	  case n if n.isArray => n.getElements.map(LEPS1dComponent(_)).toList
  	  case n if n.isObject => List(LEPS1dComponent(n))
  	  case _ => List.empty
	}

}


case class LEPS1dComponent (val coeff:Option[Double], val constant:Option[Double], val minRange:Option[Double], val maxRange:Option[Double]) {
	 
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
	
	def toString(variable:String) = coeff.asDoubleOr("0") + " * " + variable + " + " + constant.asPercentOr("0") + " for [" + 
							minRange.asDoubleOr("") + ", " + maxRange.asDoubleOr("") + "]"
	
}

object LEPS1dComponent {
	
	def apply(subnode:JsonNode):LEPS1dComponent = {
	  val coeff:Option[Double] = subnode.parseJsonDouble("mult")
	  val constant:Option[Double] = subnode.parseJsonDouble("add")
	  val minRange:Option[Double] = subnode.parseJsonDouble("minrange")
	  val maxRange:Option[Double] = subnode.parseJsonDouble("maxrange")
	  LEPS1dComponent(coeff, constant, minRange, maxRange)
	}
}
