package squantlib.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import squantlib.util.FormulaParser
import java.util.{Map => JavaMap}

/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 *  {type:"binary", variable:[string], payoff:[{amount:double, strike:[double]}], description:String}, 
 * No strike is considered as no low boundary
 */
case class BinaryPayoff(binaryVariables:List[String], payoff:List[(Double, Option[List[Double]])], description:String = null) 
extends Payoff {
  
	val variables = binaryVariables.toSet
  
	def getFixings(fixings:Map[String, Double]):Option[List[Double]] = 
	  if (variables.toSet subsetOf fixings.keySet) 
	    Some((0 to binaryVariables.size - 1).toList.map(i => fixings(binaryVariables(i))))
	  else None
	    
	override def price(fixings:Map[String, Double]) = 
	  if (payoff.isEmpty) Double.NaN
	  else getFixings(fixings) match {
	    case None => Double.NaN
	    case Some(fixValues) => 
	      payoff.map{
	        case (v, None) => v
	        case (v, Some(l)) if fixValues.corresponds(l) {_ >= _} => v
	        case _ => 0.0}.max
	  }
	  
	override def price(fixing:Double)(implicit d:DummyImplicit) =
	  if (payoff.isEmpty || variables.size != 1) Double.NaN
	  else payoff.map{
	    case (v, None) => v 
	    case (v, Some(l)) if fixing > l.head => v
	    case _ => 0.0}.max
	
	override def toString =
	  if (payoff.isEmpty) description
	  else payoff.map{
	      case (v, None) => v.asPercent + " [Const]"
	      case (v, Some(s)) => v.asPercent + " [" + s.mkString(",") + "]"
	    }.mkString(" ")
	
	override def price = Double.NaN
	
	override val jsonString = {
	  
	  val jsonPayoff:Array[JavaMap[String, Any]] = payoff.map{
	    case (v, None) => val jmap:JavaMap[String, Any] = Map("amount" -> v); jmap
	    case (v, Some(s)) => val jmap:JavaMap[String, Any] = Map("amount" -> v, "strike" -> s.toArray); jmap
	  }.toArray
	  
	  val varSet:java.util.List[String] = scala.collection.mutable.ListBuffer(binaryVariables: _*)
	    
	  val infoMap:JavaMap[String, Any] = Map(
	      "type" -> "binary",
	      "variable" -> varSet, 
	      "description" -> description,
	      "payoff" -> jsonPayoff)
	  
	  (new ObjectMapper).writeValueAsString(infoMap)	  
	}	
	
}

object BinaryPayoff {
  
	def apply(node:String):BinaryPayoff = {
	  val variable:List[String] = node.jsonNode("variable") match {
	    case Some(n) if n isArray => n.map(s => s.parseJsonString).toList
	    case Some(n) if n isTextual => List(n.asText)
	    case _ => List.empty
	  }
	  
	  val payoff:List[(Double, Option[List[Double]])] = (node.jsonNode("payoff") match {
	    case None => List.empty[(Double, Option[List[Double]])]
	    case Some(subnode) if subnode isArray => subnode.map(n => {
	      val amount = n.parseJsonDouble("amount").getOrElse(Double.NaN)
	      if (n.get("strike") == null) (amount, None)
	      else (amount, Some(n.get("strike").map(s => s.parseJsonDouble.getOrElse(Double.NaN)).toList))
	    })
	  }).toList
	  
	  val description:String = node.parseJsonString("description")
	  BinaryPayoff(variable, payoff, description)
	}
  
}