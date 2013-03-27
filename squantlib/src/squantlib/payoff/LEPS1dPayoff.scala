package squantlib.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import squantlib.util.FormulaParser
import java.util.{Map => JavaMap}
import squantlib.util.UnderlyingInfo

/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 * - {type:"leps1d", variable:string, description:String, payoff:formula}, where
 *   formula = Array {minrange:double, maxrange:double, mult:double, add:double}
 *   payment for array(i) is 
 *     if minrange(i) <= X < maxrange(i) => mult(i) * variable + add(i)
 *     otherwise zero
 */
case class LEPS1dPayoff(variable:String, payoff:List[LEPS1dComponent], description:String = null) extends Payoff {
  
	override val variables:Set[String] = Set(variable)
	  
	override def price(fixings:Map[String, Double]) = 
	  if (fixings contains variable) price(fixings(variable))
	  else Double.NaN
	
	override def price(fixing:Double) = payoff.map(_.price(fixing)).sum
	override def toString = payoff.map(p => p.toString(variable)).mkString(" ")
	
	override def price = Double.NaN
	
	override def display(isRedemption:Boolean):String = {
	  val varname = UnderlyingInfo.nameJpn(variable)
	  val vardisp = (v:Double) => UnderlyingInfo.displayValue(variable, v)
	  
	  if (isRedemption) {
	    "最終参照日の" + varname + "によって、下記の金額で償還されます。" + 
	    sys.props("line.separator") + 
	    payoff.sortBy{case LEPS1dComponent(_, _, m, _) => m.getOrElse(-9999.0)}.map(p => 
	    (p match {
	      case LEPS1dComponent(_, _, Some(min), Some(max)) => 
	        varname + "が" + vardisp(min) + "以上、" + vardisp(max) + "未満の場合 ： "
	      case LEPS1dComponent(_, _, None, Some(max)) => 
	        varname + "が" + vardisp(max) + "未満の場合 ： "
	      case LEPS1dComponent(_, _, Some(min), None) => 
	        varname + "が" + vardisp(min) + "以上の場合 ： "
	      case LEPS1dComponent(_, _, None, None) => 
	        "指数に関わらず ： "
	    }) +  
	      (p match {
	      case LEPS1dComponent(None, None, _, _) => "額面 " + (0.0).asPercent
	      case LEPS1dComponent(Some(coeff), None, _, _) => "額面 x " + coeff.asDouble + " * " + varname
	      case LEPS1dComponent(None, Some(const), _, _) => "額面 " + const.asPercent
	      case LEPS1dComponent(Some(coeff), Some(const), _, _) => "額面 x (" + coeff.asDouble + " * " + varname + (if(const < 0) " - " else " + ") + math.abs(const).asPercent + ")"
	    })).mkString(sys.props("line.separator"))
	  }
	  
	  else {
	    "利率決定日の" + varname + "によって決定されます。" + 
	    sys.props("line.separator") + 
	    payoff.sortBy{case LEPS1dComponent(_, _, m, _) => m.getOrElse(-9999.0)}.map(p => 
	    (p match {
	      case LEPS1dComponent(_, _, Some(min), Some(max)) => 
	        varname + "が" + vardisp(min) + "以上、" + vardisp(max) + "未満の場合 ： "
	      case LEPS1dComponent(_, _, None, Some(max)) => 
	        varname + "が" + vardisp(max) + "未満の場合 ： "
	      case LEPS1dComponent(_, _, Some(min), None) => 
	        varname + "が" + vardisp(min) + "以上の場合 ： "
	      case LEPS1dComponent(_, _, None, None) => 
	        "指数に関わらず ： "
	    }) + 
	      (p match {
	      case LEPS1dComponent(None, None, _, _) => (0.0).asPercent
	      case LEPS1dComponent(Some(coeff), None, _, _) => coeff.asDouble + " * " + varname
	      case LEPS1dComponent(None, Some(const), _, _) => const.asPercent
	      case LEPS1dComponent(Some(coeff), Some(const), _, _) => coeff.asDouble + " * " + varname + (if(const < 0) " - " else " + ") + math.abs(const).asPercent
	    }) + " （年率）").mkString(sys.props("line.separator"))
	  }
	}
	
	override def jsonString = {
	  
	  val jsonPayoff:Array[JavaMap[String, Any]] = payoff.toArray.map(p => {
	    val leg:JavaMap[String, Any] = Map(
	      "minrange" -> p.minRange.getOrElse("None"),
	      "maxrange" -> p.maxRange.getOrElse("None"),
	      "mult" -> p.coeff.getOrElse("None"),
	      "add" -> p.constant.getOrElse("None")
	      )
	    leg})
	    
	  val infoMap:JavaMap[String, Any] = Map(
	      "type" -> "leps1d", 
	      "variable" -> variable, 
	      "description" -> description,
	      "payoff" -> jsonPayoff)
	  
	  (new ObjectMapper).writeValueAsString(infoMap)	  
	}	
	
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
	    val variable:String = formula.parseJsonString("variable").orNull
	    val description:String = formula.parseJsonString("description").orNull
	  
	    val payoff:List[LEPS1dComponent] = formula.jsonNode match {
	      case Some(node) => getLEPScomponents(node.get("payoff"))
	      case None => List.empty
	    }
	    
	    LEPS1dPayoff(variable, payoff, description)
	  }
	}

	def apply(variable:String, payoff:JsonNode, description:String):LEPS1dPayoff = 
	  LEPS1dPayoff(variable, getLEPScomponents(payoff), description)
	
	def getLEPScomponents(node:JsonNode):List[LEPS1dComponent] = node.parseList.map(LEPS1dComponent(_))
	  
}


case class LEPS1dComponent (coeff:Option[Double], constant:Option[Double], minRange:Option[Double], maxRange:Option[Double]) {
	 
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
	
	def toString(variable:String) = 	  
	  "[" + minRange.asDoubleOr("") + ", " + maxRange.asDoubleOr("") + "] " + linearFormula(coeff, variable, constant)
	
}

object LEPS1dComponent {
	
	def apply(subnode:JsonNode):LEPS1dComponent = {
	  val coeff:Option[Double] = Some(subnode.parseDouble("mult").getOrElse(1.0))
	  val constant:Option[Double] = subnode.parseDouble("add")
	  val minRange:Option[Double] = subnode.parseDouble("minrange")
	  val maxRange:Option[Double] = subnode.parseDouble("maxrange")
	  LEPS1dComponent(coeff, constant, minRange, maxRange)
	}
}
