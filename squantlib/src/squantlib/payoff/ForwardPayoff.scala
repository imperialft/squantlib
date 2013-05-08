package squantlib.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import squantlib.util.FormulaParser
import squantlib.util.UnderlyingInfo
import java.util.{Map => JavaMap}

/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 *  {type:"forward", variable:[String], trigger:[Double], strike:[Double], description:String}, 
 * No strike is considered as no low boundary
 */
case class ForwardPayoff(fwdVariables:List[String], strike:List[Double], description:String = null) 
extends Payoff {
  
	val variables = fwdVariables.toSet
  
	def getFixings(fixings:Map[String, Double]):Option[List[Double]] = 
	  if (variables.toSet subsetOf fixings.keySet) 
	    Some((0 to fwdVariables.size - 1).toList.map(i => fixings(fwdVariables(i))))
	  else None
	    
	override def price(fixings:Map[String, Double]) = 
	  getFixings(fixings) match {
	    case Some(fixValues) if fixValues.forall(!_.isNaN) => (fixValues, strike).zipped.map((v, k) => v/k).min
	    case None => Double.NaN
	  }
	  
	override def price(fixing:Double) =
	  if (variables.size != 1 || fixing.isNaN) Double.NaN
	  else fixing / strike.head
	
	override def toString =
	  "Min{[" + variables.mkString(",") + "] / [" + strike.mkString(",") + "]}"
	
	override def price = Double.NaN
	
	override def jsonString = {
	  
	  val infoMap:JavaMap[String, Any] = Map(
	      "type" -> "forward", 
	      "variable" -> fwdVariables.toArray, 
	      "strike" -> strike.toArray, 
	      "description" -> description)
	  
	  (new ObjectMapper).writeValueAsString(infoMap)	  
	}	
	
	override def display(isRedemption:Boolean):String = {
 	  val varnames = fwdVariables.map(UnderlyingInfo.nameJpn)
	  val strikeMap = (fwdVariables, strike).zipped.map{case (v, k) => (UnderlyingInfo.nameJpn(v), UnderlyingInfo.displayValue(v, k))}
	  val multiple = variables.size > 1
	  
	  if (isRedemption) {
	    if (multiple) 
	      (List("最終参照日の" + varnames.mkString("、") + "によって、下記の低い（パフォーマンスが悪い）ほうの金額が支払われます。") ++ 
	          strikeMap.map{case (v, k) => "・ 額面 x " + v + " / " + k}).mkString(sys.props("line.separator"))
	    else
	      List("最終参照日の" + varnames.head + "によって決定されます。", 
	          strikeMap.head match {case (v, k) => "額面 x " + v + " / " + k}).mkString(sys.props("line.separator"))
	  }
	  else {
	    if (multiple) 
	      (List("利率決定日の" + varnames.mkString("、") + "によって、下記の低い（パフォーマンスが悪い）ほうの金額が支払われます。") ++ 
	          strikeMap.map{case (v, k) => "・ " + v + " / " + k + " （年率）"}).mkString(sys.props("line.separator"))
	    else
	      List("利率決定日の" + varnames.head + "によって決定されます。", 
	          strikeMap.head match {case (v, k) => "額面 x " + v + " / " + k + " （年率）"}).mkString(sys.props("line.separator"))
	  }
	    
	}
	
}

object ForwardPayoff {
  
	def apply(node:String):ForwardPayoff = {
	  
	  val variable:List[String] = node.parseJsonStringList("variable").map(_.orNull)
	  val strike:List[Double] = node.parseJsonDoubleList("strike").map(_.getOrElse(Double.NaN))
	  val description:String = node.parseJsonString("description").orNull
	  ForwardPayoff(variable, strike, description)
	}
  
}

