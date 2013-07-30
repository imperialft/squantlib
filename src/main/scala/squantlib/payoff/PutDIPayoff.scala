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
 *  {type:"putdi", variable:[String], trigger:[Double], strike:[Double], description:String}, 
 * No strike is considered as no low boundary
 */
case class PutDIPayoff(
    putVariables:List[String], 
    trigger:List[Double], 
    strike:List[Double], 
    amount:Double = 1.0, 
    description:String = null) extends Payoff {
  
	val variables = putVariables.toSet
	
	override val isPriceable:Boolean = !trigger.exists(v => v.isNaN || v.isInfinity) && !strike.exists(v => v.isNaN || v.isInfinity)
	
	def getFixings(fixings:Map[String, Double]):Option[List[Double]] = 
	  if (variables.toSet subsetOf fixings.keySet) 
	    Some((0 to putVariables.size - 1).toList.map(i => fixings(putVariables(i))))
	  else None
	    
	override def priceImpl(fixings:Map[String, Double]) = 
	  getFixings(fixings) match {
	    case Some(fixValues) if fixValues.forall(v => !v.isNaN && !v.isInfinity) => 
	      if (fixValues.corresponds(trigger) {_ >= _}) amount
	      else amount * math.min(1.00, (fixValues, strike).zipped.map((v, k) => v/k).min)
	    case None => Double.NaN
	  }
	  
	override def priceImpl(fixing:Double) =
	  if (variables.size != 1 || fixing.isNaN || fixing.isInfinity) Double.NaN
	  else if (fixing >= trigger.head) amount
	  else amount * math.min(1.00, fixing / strike.head)
	 
	override def toString =
	  amount.asPercent + " [" + trigger.mkString(",") + "] " + amount.asPercent + " x Min([" + variables.mkString(",") + "] / [" + strike.mkString(",") + "])"
	
	override def priceImpl = Double.NaN
	
	override def display(isRedemption:Boolean):String = {
 	  val varnames = putVariables.map(UnderlyingInfo.nameJpn)
	  val strikeMap = (putVariables, strike).zipped.map{case (v, k) => (UnderlyingInfo.nameJpn(v), UnderlyingInfo.displayValue(v, k))}
	  val triggerMap = (putVariables, trigger).zipped.map{case (v, t) => (UnderlyingInfo.nameJpn(v), UnderlyingInfo.displayValue(v, t))}
	  val multiple = variables.size > 1
	  
	  if (isRedemption){
	    List(
	        "・" + (if(multiple) "全ての参照指数" else varnames.head) + "がノックイン価格を上回っている場合 ： 額面 " + amount.asPercent,
	        "・" + (if(multiple) "いずれかの参照指数" else varnames.head) + "がノックイン価格を下回っている場合 ： ",
	        "  " + strikeMap.map{case (v, k) => "額面 x " + v + " / " + k}.mkString("、") + (if(multiple) "の低いほう" else ""),
	        "",
	        "ノックイン価格 ： " + triggerMap.map{case (v, k) => v + " ＝ " + k}.mkString("、"))
	        .mkString(sys.props("line.separator"))
	  }

	  else {
	    List(
	        "・" + (if(multiple) "全ての参照指数" else varnames.head) + "がノックイン価格を上回っている場合 ： " + amount.asPercent + " (年率）",
	        "・" + (if(multiple) "いずれかの参照指数" else varnames.head) + "がノックイン価格を下回っている場合 ： ",
	        "  " + strikeMap.map{case (v, k) => amount.asPercent + " x " + v + " / " + k}.mkString("、") + (if(multiple) "の低いほう" else "")  + " (年率）",
	        "",
	        "ノックイン価格 ： " + triggerMap.map{case (v, k) => v + " ＝ " + k}.mkString("、"))
	        .mkString(sys.props("line.separator"))
	  }
	}
	
	override def jsonString = {
	  
	  val infoMap:JavaMap[String, Any] = Map(
	      "type" -> "putdi", 
	      "variable" -> putVariables.toArray, 
	      "trigger" -> trigger.toArray, 
	      "strike" -> strike.toArray, 
	      "description" -> description)
	  
	  (new ObjectMapper).writeValueAsString(infoMap)	  
	}	
	
}

object PutDIPayoff {
  
	def apply(node:String):PutDIPayoff = {
	  
	  val variable:List[String] = node.parseJsonStringList("variable").map(_.orNull)  
	  val trigger:List[Double] = node.parseJsonDoubleList("trigger").map(_.getOrElse(Double.NaN))
	  val strike:List[Double] = node.parseJsonDoubleList("strike").map(_.getOrElse(Double.NaN))
	  val amount:Double = node.parseJsonDouble("amount").getOrElse(1.0)
	  val description:String = node.parseJsonString("description").orNull
	  PutDIPayoff(variable, trigger, strike, amount, description)
	}
  
}

