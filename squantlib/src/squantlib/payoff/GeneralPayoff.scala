package squantlib.payoff

import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import squantlib.util.FormulaParser

/**
 * Interprets general formula as the sum of weighted variables + constant, with cap and floor.
 * Any symbols other than +, -, *, /, > and < are considered as an independent variable.
 * Brackets are not supported.
 */
case class GeneralPayoff(val formula:Map[Set[String], Double], val floor:Option[Double], val cap:Option[Double], val description:String = null) extends Payoff {
	
	override val variables:Set[String] = formula.keySet.flatten
	 
	def leverage(index:String):Double = leverage(Set(index))
	def leverage(index:Set[String]):Double = formula.getOrElse(index, 0.0)
	
	val constant:Double = formula.getOrElse(Set.empty, 0.0)
	
	override def price(fixing:Double) (implicit d:DummyImplicit) = 
	  if (variables.size == 1) price(Map(variables.head -> fixing))
	  else Double.NaN
	
	override def price(fixings:Map[String, Double]):Double = {
	  if (!variables.forall(x => fixings.contains(x))) return Double.NaN
	  
	  var rate = formula.map{
      	case (vs, c) if vs.isEmpty => c
      	case (vs, c) => vs.toList.map(x => fixings(x)).product * c}.sum
    
      if (floor.isDefined) rate = rate.max(floor.get)
      if (cap.isDefined) rate = rate.min(cap.get)
	
      rate
	}
	 
	override def price = 
	  if (variables.isEmpty) constant
	  else Double.NaN
	
	override def toString:String = 
	  	  formula.map{case (variables, coeff) => 
	    ((if (variables.size > 0) variables.mkString("*") + "*" + coeff.toDouble else coeff.asPercent) )}.mkString("+").replace("+-", "+")

	
	override val jsonString = formula.map{
	  case (variables, coeff) => 
	    ((if (variables.size > 0) variables.mkString("*") + "*" + coeff else coeff) )}.mkString("+").replace("+-", "+")
	
}

import scala.collection.JavaConversions._

object GeneralPayoff {
  
	def apply(inputstring:String):Payoff = {

	  val formula = if (inputstring.startsWith("{") && inputstring.parseJsonString("type") == "general") {
	    var f = inputstring.parseJsonString("payoff") match { case null => ""; case n => n}
	    inputstring.jsonNode match {
	      case Some(node) => 
	        val fieldnames = node.getFieldNames.filter(n => !List("type", "description", "payoff", "variable").contains(n))
	        fieldnames.foreach(n => f = f.replace(n, node.get(n).parseJsonDouble.getOrElse(Double.NaN).toString))
	      case None => {}
	    }
	    f
	  } else inputstring
	  
	  val description =  if (inputstring.startsWith("{")) inputstring.parseJsonString("description") else null
	  
	  val (parsedformula, floor, cap) = FormulaParser.parse(formula)
	  
	  val variables = parsedformula.keySet.flatten
	  val constant = parsedformula.getOrElse(Set.empty, 0.0)
	  
	  variables.size match {
	    case 0 => FixedPayoff(constant, description)
	    case 1 => {
	      val variable = variables.head
	      Linear1dPayoff(variable, Some(parsedformula.getOrElse(Set(variable), 0.0)), Some(constant), floor, cap, description)
	    }
	    case _ => GeneralPayoff(parsedformula, floor, cap, description)
	  }
	  
	}

}

