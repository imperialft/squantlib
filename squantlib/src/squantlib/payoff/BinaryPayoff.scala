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
	
	override def jsonString = {
	  
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
	    
	override def display(isRedemption:Boolean):String = {
	  val varnames = binaryVariables.map(v => (v, UnderlyingInfo.nameJpn(v))).toMap
	  val dispValue = (v:String, s:Double) => UnderlyingInfo.displayValue(v, s)
	  
	  if (isRedemption)
	    "最終参照日の参照価格によって決定されます。" + sys.props("line.separator") + 
	    payoff.sortBy{case (amt, stk) => amt}.reverse.map{
	      case (amt, Some(stks)) => (binaryVariables, stks).zipped.map{
	      case (v, s) => "・ " + varnames(v) + "が " + dispValue(v, s) + "以上"}.mkString("、") + "の場合 ： 額面 " + amt.asPercent
	      case (amt, None) => "・ それ以外の場合 ： 額面 "+ amt.asPercent
	    }.mkString(sys.props("line.separator"))
	  
	  else
	    "利率決定日の参照価格によって決定されます。" + sys.props("line.separator") + 
	    payoff.sortBy{case (amt, stk) => amt}.reverse.map{
	      case (amt, Some(stks)) => (binaryVariables, stks).zipped.map{
	      case (v, s) => "・ " + varnames(v) + "が " + dispValue(v, s) + "以上"}.mkString("、") + "の場合 ： 年率 " + amt.asPercent
	      case (amt, None) => "・ それ以外の場合 ： 年率 "+ amt.asPercent
	    }.mkString(sys.props("line.separator"))
	}
}

object BinaryPayoff {
  
	def apply(node:String):BinaryPayoff = {
	  val variable:List[String] = node.parseJsonStringList("variable").map(_.orNull)
	  
	  val payoff:List[(Double, Option[List[Double]])] = (node.jsonNode("payoff") match {
	    case None => List.empty
	    case Some(subnode) if subnode isArray => subnode.map(n => {
	      val amount = n.parseDouble("amount").getOrElse(Double.NaN)
	      if (n.get("strike") == null) (amount, None)
	      else (amount, Some(n.get("strike").map(s => s.parseDouble.getOrElse(Double.NaN)).toList))
	    })
	    case _ => List.empty
	  }).toList
	  
	  val description:String = node.parseJsonString("description").orNull
	  BinaryPayoff(variable, payoff, description)
	}
  
}