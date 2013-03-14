package squantlib.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import squantlib.util.FormulaParser
import java.util.{Map => JavaMap}
import org.jquantlib.time.{Date => qlDate}
import squantlib.database.fixings.Fixings
import squantlib.util.UnderlyingInfo

/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 *  {type:"putdiamerican", variable:[String], trigger:[Double], strike:[Double], refStart:Date, refEnd:Date, description:String}, 
 * No strike is considered as no low boundary
 */
case class PutDIAmericanPayoff(
    putVariables:List[String], 
    trigger:List[Double], 
    strike:List[Double], 
    refStart:qlDate, 
    refEnd:qlDate, 
    knockedIn:Boolean, 
    amount:Double = 1.0, 
    description:String = null) extends Payoff {
  
	val variables = putVariables.toSet
	
	var mcPeriod = 30
	
	override def eventDates(period:CalculationPeriod):List[qlDate] = {
	  val basemod = refEnd.serialNumber % mcPeriod
	  val dates = for (i <- (refStart.serialNumber to refEnd.serialNumber).toList if i % mcPeriod == basemod) yield new qlDate(i)
	  if (dates.head == refStart) dates else refStart :: dates
	}
	
	var barrierCoeff:() => Double = () => 1.0
  
	def getFixings(fixings:Map[String, Double]):Option[List[Double]] = {
	  if (variables.toSet subsetOf fixings.keySet) 
	    Some((0 to putVariables.size - 1).toList.map(i => fixings(putVariables(i))))
	  else None
	}
	
	def isKnockIn(fixings:Map[String, Double]):Boolean = getFixings(fixings) match {
	  case Some(fixing) => (fixing, trigger).zipped.exists(_ <= _)
	  case None => false
	}
	
	def isKnockIn(fixings:List[Map[String, Double]]):Boolean = fixings.exists(isKnockIn(_))
	
	override def price(fixings:Map[String, Double]):Double = price(fixings, knockedIn)
	
	override def price(fixings:List[Map[String, Double]]):Double = fixings.size match {
	  case 0 => price
	  case 1 => price(fixings.head, knockedIn)
	  case _ => price(fixings.last, knockedIn || isKnockIn(fixings.dropRight(1)))
	}
	    
	def price(fixings:Map[String, Double], ki:Boolean):Double = 
	  getFixings(fixings) match {
	    case None => Double.NaN
	    case Some(fixValues) if ki => amount * math.min(1.00, (fixValues, strike).zipped.map((v, k) => v/k).min)
	    case Some(fixValues) if barrierCoeff != null => val coeff = barrierCoeff()
	    	amount * ((1.0 - coeff) * 1.0 + coeff * math.min(1.00, (fixValues, strike).zipped.map((v, k) => v/k).min))
	    case _ => Double.NaN
	  }
	
	  
	override def price(fixing:Double):Double =
	  if (variables.size != 1) Double.NaN
	  else if (knockedIn) amount * math.min(1.0, fixing / strike.head)
	  else if (barrierCoeff == null) Double.NaN
	  else {
	    val coeff = barrierCoeff()
	    amount * ((1.0 - coeff) * 1.0 + coeff * math.min(1.00, fixing / strike.head))
	  }
	
	override def toString =
	  amount.asPercent + " [" + trigger.mkString(",") + "](Amer) " + amount.asPercent + " x Min([" + variables.mkString(",") + "] / [" + strike.mkString(",") + "]"
	
	override def price = Double.NaN
	
	override def display(isRedemption:Boolean):String = {
 	  val varnames = putVariables.map(UnderlyingInfo.nameJpn)
	  val strikeMap = (putVariables, strike).zipped.map{case (v, k) => (UnderlyingInfo.nameJpn(v), UnderlyingInfo.displayValue(v, k))}
	  val triggerMap = (putVariables, trigger).zipped.map{case (v, t) => (UnderlyingInfo.nameJpn(v), UnderlyingInfo.displayValue(v, t))}
	  val multiple = strike.size > 1
	  val start = if (refStart == null) "" else "%tY年%<tm月%<td日".format(refStart.longDate)
	  val end = if (refEnd == null) "" else "%tY年%<tm月%<td日".format(refEnd.longDate)
	  
	  if (isRedemption) {
	      List("判定期間中の" + varnames.mkString("、") + "に応じて、最終参照日に決定されます。", 
	        "・" + (if(multiple) "全ての参照指数" else varnames.head) + "が常にノックイン価格を上回った場合 ： " + (if (knockedIn) "(ノックイン済み）" else "額面 " + amount.asPercent),
	        "・" + (if(multiple) "いずれかの参照指数" else varnames.head) + "が一度でもノックイン価格を下回った場合 ： ",
	        "  " + strikeMap.map{case (v, k) => "額面 x " + v + " / " + k}.mkString("、") + (if(multiple) "の低いほう" else ""),
	        "",
	        "ノックイン価格 ： " + triggerMap.map{case (v, k) => v + " ＝ " + k}.mkString("、"),
	        "判定期間 ： " + start + " ～ " + end)
	        .mkString(sys.props("line.separator"))
	  }
	  
	  else {
	    List("判定期間中の" + varnames.mkString("、") + "に応じて、利率決定日に決定されます。", 
	        "・" + (if(multiple) "全ての参照指数" else varnames.head) + "が常にノックイン価格を上回った場合 ： " + (if (knockedIn) "(ノックイン済み）" else amount.asPercent),
	        "・" + (if(multiple) "いずれかの参照指数" else varnames.head) + "が一度でもノックイン価格を下回った場合 ： ",
	        "  " + strikeMap.map{case (v, k) => amount.asPercent + " x " + v + " / " + k + " （年率）"}.mkString("、") + (if(multiple) "の低いほう" else ""),
	        "",
	        "ノックイン価格 ： " + triggerMap.map{case (v, k) => v + " ＝ " + k}.mkString("、"),
	        "判定期間 ： " + start + " ～ " + end)
	        .mkString(sys.props("line.separator"))
	  }
	}
	
	override def jsonString = {
	  
	  val infoMap:JavaMap[String, Any] = Map(
	      "type" -> "putdiamerican", 
	      "variable" -> putVariables.toArray, 
	      "trigger" -> trigger.toArray, 
	      "strike" -> strike.toArray,
	      "refstart" -> (if (refStart == null) null else "%tY/%<tm/%<td".format(refStart.longDate)),
	      "refend" -> (if (refEnd == null) null else "%tY/%<tm/%<td".format(refEnd.longDate)),
	      "description" -> description)
	  
	  (new ObjectMapper).writeValueAsString(infoMap)	  
	}	
	
}

object PutDIAmericanPayoff {
  
	def apply(node:String):PutDIAmericanPayoff = {
	  
	  val variable:List[String] = node.parseJsonStringList("variable").map(_.orNull)
	  val trigger:List[Double] = node.parseJsonDoubleList("trigger").map(_.getOrElse(Double.NaN))
	  val strike:List[Double] = node.parseJsonDoubleList("strike").map(_.getOrElse(Double.NaN))
	  val amount:Double = node.parseJsonDouble("amount").getOrElse(1.0)
	  val refStart:qlDate = node.parseJsonDate("refstart").orNull
	  val refEnd:qlDate = node.parseJsonDate("refend").orNull
	  val description:String = node.parseJsonString("description").orNull
	  
	  val knockedIn:Boolean = 
	    if (refStart == null || refEnd == null) false
	    else (variable zip trigger).exists{case (v, trig) => Fixings.getHistorical(v, refStart, refEnd) match {
	      case Some(h) => h.exists{case (_, x) => x <= trig}
	      case None => false
	    }}
	  
	  PutDIAmericanPayoff(variable, trigger, strike, refStart, refEnd, knockedIn, amount, description)
	}
  
}

