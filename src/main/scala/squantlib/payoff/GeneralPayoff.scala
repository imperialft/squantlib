package squantlib.payoff

import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import squantlib.util.FormulaParser

/**
 * Interprets general formula as the sum of weighted variables + constant, with cap and floor.
 * Any symbols other than +, -, *, /, > and < are considered as an independent variable.
 * Brackets are not supported.
 */
case class GeneralPayoff(
    formula:Map[Set[String], Double], 
    floor:Option[Double], 
    cap:Option[Double], 
    description:String = null) extends Payoff {
	
	override val variables:Set[String] = formula.keySet.flatten
	
	override val isPriceable = formula.values.forall(v => !v.isNaN && !v.isInfinity)
	
	override val isFixed = variables.size == 0 || super.isFixed
	
	def leverage(index:String):Double = leverage(Set(index))
	def leverage(index:Set[String]):Double = formula.getOrElse(index, 0.0)
	
	val constant:Double = formula.getOrElse(Set.empty, 0.0)
	
	override def priceImpl(fixing:Double) = 
	  if (variables.size == 1 && !fixing.isNaN && !fixing.isInfinity) price(Map(variables.head -> fixing))
	  else Double.NaN
	
	override def priceImpl(fixings:Map[String, Double]):Double = {
	  if (!(variables subsetOf fixings.keySet) || variables.exists(v => fixings(v).isNaN || fixings(v).isInfinity)) {return Double.NaN}
	  
	  var rate = formula.map{
      	case (vs, c) if vs.isEmpty => c
      	case (vs, c) => vs.toList.map(fixings).product * c}.sum
    
      if (floor.isDefined) rate = rate.max(floor.get)
      if (cap.isDefined) rate = rate.min(cap.get)
	
      rate
	}
	 
	override def priceImpl = 
	  if (variables.isEmpty) constant
	  else Double.NaN
	
	override def toString:String = 
	  	  formula.map{case (variables, coeff) => 
	    ((if (variables.size > 0) variables.mkString("*") + "*" + coeff.toDouble else coeff.asPercent) )}.mkString("+").replace("+-", "+")

	override def jsonString = formula.map{
	  case (variables, coeff) => 
	    ((if (variables.size > 0) variables.mkString("*") + "*" + coeff else coeff) )}.mkString("+").replace("+-", "+")
	
}

import scala.collection.JavaConversions._

object GeneralPayoff {
  
	def apply(inputstring:String):Payoff = {

	  val formula = if (inputstring.startsWith("{") && inputstring.parseJsonString("type") == Some("general")) {
	    var f = inputstring.parseJsonString("payoff") match { case None => ""; case Some(n) => n}
	    inputstring.jsonNode match {
	      case Some(node) => 
	        val fieldnames = node.getFieldNames.filter(n => !List("type", "description", "payoff", "variable").contains(n))
	        fieldnames.foreach(n => f = f.replace(n, node.get(n).parseDouble.getOrElse(Double.NaN).toString))
	      case None => {}
	    }
	    f
	  } else inputstring
	  
	  val description =  if (inputstring.startsWith("{")) inputstring.parseJsonString("description").orNull else null
	  
	  val (parsedformula, floor, cap) = FormulaParser.parse(formula)
	  
	  val variables = parsedformula.keySet.flatten
	  val constant = parsedformula.getOrElse(Set.empty, 0.0)
	  
	  variables.size match {
	    case 0 if parsedformula contains Set.empty => FixedPayoff(constant, description)
	    case 0 => NullPayoff(description, inputstring)
	    case 1 => {
	      val variable = variables.head
	      Linear1dPayoff(variable, Some(parsedformula.getOrElse(Set(variable), 0.0)), Some(constant), floor, cap, description)
	    }
	    case _ => GeneralPayoff(parsedformula, floor, cap, description)
	  }
	  
	}

}

