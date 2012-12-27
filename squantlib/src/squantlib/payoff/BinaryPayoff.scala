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
 *  {type:"binary", variable:string, payoff:[{strike:double, amount:double}], description:String}, 
 * No strike is considered as no low boundary
 */
case class BinaryPayoff(variable:String, payoff:List[(Double, Option[Double], Option[Double])], description:String = null) 
extends Payoff {
  
	override val variables:Set[String] = Set(variable)
	
	val vertical:Set[(Option[Double], Double)] = {
	  val strikes:Set[Option[Double]] = (payoff.map(_._2) ++ payoff.map(_._3)).toSet
	  strikes.map{
	    case None => (None, price(Double.MinValue))
	    case Some(s) => (Some(s), price(s + 1e-10))
	  }
	}
	  
	override def price(fixings:Map[String, Double]) = 
	  if (fixings contains variable) price(fixings(variable))
	  else Double.NaN
	
	override def price(fixing:Double)(implicit d:DummyImplicit) =
	  if (payoff.isEmpty) Double.NaN
	  else payoff.map{
	    case (v, None, None) => v 
	    case (v, Some(l), None) if fixing > l => v
	    case (v, None, Some(h)) if fixing <= h => v
	    case (v, Some(l), Some(h)) if fixing > l && fixing <= h => v
	    case _ => 0.0}.sum
	
	override def toString = vertical.toList.sortBy(_._1.getOrElse(Double.MinValue)).reverse.map{
	      case (None, v) => v.asPercent
	      case (Some(s), v) => v.asPercent + " [" + s.asDouble + "]"
	    }.mkString(" ")
	
	override def price = Double.NaN
	
	override val jsonString = {
	  
	  val jsonPayoff:Array[JavaMap[String, Any]] = vertical.toArray.map(p => {
	    val leg:JavaMap[String, Any] = Map(
	      "strike" -> p._1.getOrElse("None"),
	      "amount" -> p._2
	      )
	    leg})
	    
	  val infoMap:JavaMap[String, Any] = Map(
	      "type" -> "binary", 
	      "variable" -> variable, 
	      "description" -> description,
	      "payoff" -> jsonPayoff)
	  
	  (new ObjectMapper).writeValueAsString(infoMap)	  
	}	
	
}

object BinaryPayoff {
  
	def apply(node:String):BinaryPayoff = {
	  val variable:String = node.parseJsonString("variable")
	  
	  val payoff:List[(Double, Option[Double])] = (node.jsonNode("payoff") match {
	    case None => List.empty
	    case Some(subnode) if subnode isArray => subnode.map(n => {
	      (n.parseJsonDouble("amount").getOrElse(Double.NaN), n.parseJsonDouble("strike"))
	    })
	  }).toList.sortBy(_._2.getOrElse(Double.MinValue))
	  
	  val binaryArray:List[(Double, Option[Double], Option[Double])] = payoff.size match {
	    case 0 => List.empty
	    case 1 => List((payoff.head._1, payoff.head._2, None))
	    case _ => 
	      val array1:List[(Double, Option[Double], Option[Double])] = (0 to payoff.size - 2).map(i => (payoff(i)._1, payoff(i)._2, payoff(i+1)._2)).toList
	      val lastelement = (payoff.last._1, payoff.last._2, None)
	      array1 :+  lastelement
	  }
	  
	  val description:String = node.parseJsonString("description")
	  BinaryPayoff(variable, binaryArray, description)
	}
  
}