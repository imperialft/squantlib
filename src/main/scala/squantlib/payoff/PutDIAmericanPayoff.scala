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
  
	override val variables = putVariables.toSet
	
	val strikeMap:Map[String, Double] = (putVariables zip strike) (collection.breakOut)
 	
	val triggerMap:Map[String, Double] = (putVariables zip trigger) (collection.breakOut)
	
	override val isPriceable:Boolean = !trigger.exists(v => v.isNaN || v.isInfinity) && !strike.exists(v => v.isNaN || v.isInfinity)
	
	var mcPeriod6m = 30
	var mcPeriod1y = 90
	var mcPeriodbefore = 180
	
	override def eventDates(period:CalculationPeriod):List[qlDate] = {
	  if (refStart == null || refEnd == null || (refStart gt refEnd)) {return List(period.endDate)}
	  val basemod = refEnd.serialNumber % mcPeriod6m
	  val start = refStart.serialNumber
	  val end = refEnd.serialNumber
	  val dates:List[qlDate] = (for (i <- (start to end) 
	      if (i >= end - 180 && i % mcPeriod6m == basemod)
	      || (i >= end - 360 && i % mcPeriod1y == basemod)
	      || (i % mcPeriodbefore == basemod)) yield new qlDate(i)) (collection.breakOut)
	  if (dates.head == refStart) dates else refStart :: dates
	}
	
	trait FixingInterpreter[T] {
	  def isKnockIn(fixings:T):Boolean // Method to be implemented
	  def price(fixings:T, isKnockedIn:Boolean):Double // Method to be implemented
	  
	  def isKnockIn(fixings:List[T]):Boolean = fixings.exists(isKnockIn(_))
	  def price(fixings:T):Double = price(fixings, isKnockIn(fixings))
	  def price(fixings:List[T]):Double = price(fixings.last, isKnockIn(fixings))
	}
	
	implicit object MapInterpreter extends FixingInterpreter[Map[String, Double]] {
	  override def isKnockIn(fixings:Map[String, Double]):Boolean = 
	    variables.exists(p => fixings.get(p) match { case Some(v) => v <= triggerMap(p) case None => false})
	  
	  override def price(fixings:Map[String, Double], isKnockedIn:Boolean):Double = {
	    if ((variables subsetOf fixings.keySet) && variables.forall(v => !fixings(v).isNaN && !fixings(v).isInfinity) && refStart != null && refEnd != null) {
	      if (isKnockedIn) amount * math.min(1.00, variables.map(v => fixings(v) / strikeMap(v)).min)
	      else amount
	    } else Double.NaN
	}}
	
	implicit object DoubleInterpreter extends FixingInterpreter[Double] {
	  override def isKnockIn(fixing:Double):Boolean = fixing <= trigger.head
	  override def price(fixing:Double, isKnockedIn:Boolean):Double = 
	    if (fixing.isNaN || fixing.isInfinity || variables.size != 1) Double.NaN
	    else if (isKnockedIn) amount * math.min(1.0, fixing / strike.head)
	    else amount
	  }
	
	def priceSingle[A:FixingInterpreter](fixings:A):Double = implicitly[FixingInterpreter[A]] price fixings
	
	def priceList[A:FixingInterpreter](fixings:List[A]):Double = implicitly[FixingInterpreter[A]] price fixings
	
	override def priceImpl(fixings:List[Map[String, Double]]):Double = priceList(fixings)

	override def priceImpl(fixings:Map[String, Double]):Double = priceSingle(fixings)
	
	override def priceImpl[T:ClassManifest](fixings:List[Double]):Double = priceList(fixings)
	
	override def priceImpl(fixing:Double):Double = priceSingle(fixing)
	
	override def priceImpl = Double.NaN
	
	override def toString =
	  amount.asPercent + " [" + trigger.mkString(",") + "](Amer) " + amount.asPercent + " x Min([" + variables.mkString(",") + "] / [" + strike.mkString(",") + "])" 
	
	override def display(isRedemption:Boolean):String = {
 	  val varnames = putVariables.map(UnderlyingInfo.nameJpn)
	  val strikeMap = (putVariables, strike).zipped.map{case (v, k) => (UnderlyingInfo.nameJpn(v), UnderlyingInfo.displayValue(v, k))}
	  val triggerMap = (putVariables, trigger).zipped.map{case (v, t) => (UnderlyingInfo.nameJpn(v), UnderlyingInfo.displayValue(v, t))}
	  val multiple = strike.size > 1
	  val start = if (refStart == null) "" else "%tY年%<tm月%<td日".format(refStart.longDate)
	  val end = if (refEnd == null) "" else "%tY年%<tm月%<td日".format(refEnd.longDate)
	  
	  if (isRedemption) {
	      List(
	        "・" + (if(multiple) "全ての参照指数" else varnames.head) + "が常にノックイン価格を上回った場合 ： " + (if (knockedIn) "(ノックイン済み）" else "額面 " + amount.asPercent),
	        "・" + (if(multiple) "いずれかの参照指数" else varnames.head) + "が一度でもノックイン価格を下回った場合 ： ",
	        "  " + strikeMap.map{case (v, k) => "額面 x " + v + " / " + k}.mkString("、") + (if(multiple) "の低いほう" else ""),
	        "",
	        "ノックイン価格 ： " + triggerMap.map{case (v, k) => v + " ＝ " + k}.mkString("、"),
	        "判定期間 ： " + start + " ～ " + end)
	        .mkString(sys.props("line.separator"))
	  }
	  
	  else {
	    List(
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

