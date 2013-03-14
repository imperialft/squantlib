package squantlib.payoff

import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import squantlib.util.FormulaParser
import squantlib.util.UnderlyingInfo

/**
 * Interprets general formula as the sum of weighted variables + constant, with cap and floor.
 * Any symbols other than +, -, *, /, > and < are considered as an independent variable.
 * Brackets are not supported.
 */
case class GeneralPayoff(formula:Map[Set[String], Double], floor:Option[Double], cap:Option[Double], description:String = null) extends Payoff {
	
	override val variables:Set[String] = formula.keySet.flatten
	 
	def leverage(index:String):Double = leverage(Set(index))
	def leverage(index:Set[String]):Double = formula.getOrElse(index, 0.0)
	
	val constant:Double = formula.getOrElse(Set.empty, 0.0)
	
	override def price(fixing:Double) = 
	  if (variables.size == 1) price(Map(variables.head -> fixing))
	  else Double.NaN
	
	override def price(fixings:Map[String, Double]):Double = {
	  if (!(variables subsetOf fixings.keySet)) {return Double.NaN}
	  
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
	    

	override def display(isRedemption:Boolean):String = 
	  if (isRedemption) {
	    "最終参照日の" + variables.mkString("、") + "によって、額面に対して下記の金額が支払われます。" + sys.props("line.separator") + 
	    formula.toList.sortBy{case (s, _) => s.size}.reverse.map{
	      case (vars, coeff) if vars.size > 0 && coeff == 1.0 => 
	        (vars.head match {
	          case v if v.take(1) == "/" => " 1 / " + UnderlyingInfo.nameJpn(v drop 1)
	          case v => UnderlyingInfo.nameJpn(v)
	        }) + vars.tail.map{
	          case v if v.take(1) == "/" => " / " + UnderlyingInfo.nameJpn(v drop 1)
	          case v => " * " + UnderlyingInfo.nameJpn(v)
	        }.mkString("")
	        
	      case (vars, coeff) if vars.size > 0 => 
	        coeff.asDouble + vars.map{ 
	          case v if v.take(1) == "/" => " / " + UnderlyingInfo.nameJpn(v drop 1)
	          case v => " * " + UnderlyingInfo.nameJpn(v)
	      }.mkString("")
	    
	      case (_, coeff) => coeff.asPercent
	    
	    }.mkString(" + ").replace("+ -", "- ") + sys.props("line.separator") + ((cap, floor) match {
	      case (Some(c), None) => "ただし" + c.asPercent + "を上回りません。"
	      case (None, Some(f)) => "ただし" + f.asPercent + "を下回りません。"
	      case (Some(c), Some(f)) => "ただし" + c.asPercent + "を上回らず、" + f.asPercent + "を下回りません。"
	      case (None, None) => ""	    
	  })}
	  
	  else {
	    "利率決定日の" + variables.mkString("、") + "によって決定されます。" + sys.props("line.separator") + 
	    formula.toList.sortBy{case (s, _) => s.size}.reverse.map{
	      case (vars, coeff) if vars.size > 0 && coeff == 1.0 => 
	        (vars.head match {
	          case v if v.take(1) == "/" => " 1 / " + UnderlyingInfo.nameJpn(v drop 1)
	          case v => UnderlyingInfo.nameJpn(v)
	        }) + vars.tail.map{
	          case v if v.take(1) == "/" => " / " + UnderlyingInfo.nameJpn(v drop 1)
	          case v => " * " + UnderlyingInfo.nameJpn(v)
	        }.mkString("")
	        
	      case (vars, coeff) if vars.size > 0 => 
	        coeff.asDouble + vars.map{ 
	          case v if v.take(1) == "/" => " / " + UnderlyingInfo.nameJpn(v drop 1)
	          case v => " * " + UnderlyingInfo.nameJpn(v)
	      }.mkString("")
	    
	      case (_, coeff) => coeff.asPercent
	    
	    }.mkString(" + ").replace("+ -", "- ") + " （年率）" + sys.props("line.separator") + ((cap, floor) match {
	      case (Some(c), None) => "ただし" + c.asPercent + "を上回りません。"
	      case (None, Some(f)) => "ただし" + f.asPercent + "を下回りません。"
	      case (Some(c), Some(f)) => "ただし" + c.asPercent + "を上回らず、" + f.asPercent + "を下回りません。"
	      case (None, None) => ""
	  })}
	    
	
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

