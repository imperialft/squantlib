package net.squantlib.schedule.call

import scala.language.postfixOps
import scala.collection.LinearSeq
import scala.collection.JavaConversions._
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.schedule.FixingLegs
import net.squantlib.util.FixingInformation
import net.squantlib.schedule.Schedule
import net.squantlib.schedule.payoff.Payoffs
import org.codehaus.jackson.JsonNode

case class Callabilities(calls:List[Callability]) extends LinearSeq[Callability] with FixingLegs[Callability]{
  
	val underlyings:Set[String] = calls.map(_.underlyings).flatten.toSet
	
	def bermudans:List[Boolean] = calls.map(_.bermudan)
	
	def triggers:List[Option[Map[String, Double]]] = calls.map(c => 
	  if (c.isTriggered) Some(c.triggers.map{case (k, v) => (k, 0.0)})
	  else if (c.isFixed) None
	  else if (c isTrigger) Some(c.triggers) 
	  else None)
	
	def triggerValues(variables:List[String]):List[List[Option[Double]]] = calls.map(_.triggerValues(variables))
	
	def isTargetRedemption = calls.exists(c => c.targetRedemption.isDefined)

	def targetRedemptions:List[Option[Double]] = calls.map(_.targetRedemption)
	
	def isTriggeredByTrigger:Boolean = calls.exists(c => c.isFixed && c.fixedTriggerByTrigger == Some(true))

  def isTriggeredByTarget:Boolean = calls.exists(c => c.fixedTriggerByTargetRedemption == Some(true))

	def isTriggered:Boolean = calls.exists(c => c.fixedTrigger == Some(true))
  
	val isPriceable:Boolean = calls.forall(_.isPriceable)
	
	val bonus:List[Double] = calls.map(_.bonus)
	
	def fill(legs:Int) = size match {
	  case l if l > legs => Callabilities(this.takeRight(legs))
	  case l if l == legs => Callabilities(this)
	  case l if l < legs => Callabilities(List.fill(legs - l)(Callability.empty) ++ this)
	}
	
	def ++(another:Callabilities) = new Callabilities(calls ++ another.calls)
	
	def :+(call:Callability) = new Callabilities(calls :+ call)
	
	override def toString = calls.map(_.toString).mkString("\n")
	
  def apply(i:Int):Callability = calls(i)
    
  def isBermuda:Boolean = calls.exists(_.bermudan)
    
  val isTrigger = calls.exists(_.isTrigger) && !isTriggeredByTrigger
    
	def triggerCheck(fixings:List[Map[String, Double]]):List[Boolean] = (fixings, calls).zipped.map{case (f, c) => c.isTriggered(f)}
	
	override def isEmpty:Boolean = calls.isEmpty || calls.forall(_.isEmpty)
	
	override def head:Callability = calls.head
	
	override def tail = calls.tail
	
	override def length = calls.length
	
	override def iterator:Iterator[Callability] = calls.iterator
	
	override def toList:List[Callability] = calls
	
	def reorder(order:List[Int]) = new Callabilities((0 to calls.size-1).map(i => calls(order(i))) (collection.breakOut)) 
	
	override def isFixed:Boolean = fixinglegs.forall(_.isFixed) || isTriggered
	
	override val fixinglegs = calls
	
	def assignAccumulatedPayments(schedule:Schedule, payoffs:Payoffs) = {
    val payments = (schedule, payoffs).zipped.map{case (s, p) => (s.paymentDate, if (p.price.isNaN) 0.0 else p.price * s.dayCount)}
    (schedule, calls).zipped.foreach{case (s, c) => 
      c.accumulatedPayments = Some(payments.filter{case (d, p) => d <= s.paymentDate}.map{case (d, p) => p}.sum)
    }
	}
	
}


object Callabilities {
	
	def empty:Callabilities = new Callabilities(List.empty)
	
	def apply(calls:LinearSeq[Callability]):Callabilities = new Callabilities(calls.toList)
	
	def bermudan(formula:String, legs:Int):List[Boolean] = bermudanList(formula, legs) match {
	  case berms if !berms.isEmpty && berms.takeRight(1).head => berms.dropRight(1) :+ false
	  case berms => berms
	}
	
  def bermudanList(formula:String, nbLegs:Int):List[Boolean] = formula.jsonNode match {
    case Some(b) if b.isArray && b.size == 1 => List.fill(nbLegs - 2)(b.head.parseString == Some("berm")) ++ List(false, false)
    case Some(b) if b isArray => List.fill(nbLegs - 2 - b.size)(false) ++ b.map(_.parseString == Some("berm")).toList ++ List(false, false)
    case _ => List.fill(nbLegs)(false)
  }
  
  def target(formula:String, legs:Int):List[Option[Double]] = targetList(formula, legs) match {
    case targets if !targets.isEmpty && targets.takeRight(1).head.isDefined => targets.dropRight(1) :+ None
    case targets => targets
  }
  
  def targetList(formula:String, nbLegs:Int):List[Option[Double]] = formula.jsonNode match {
    case Some(b) if b.isArray && b.size == 1 => List.fill(nbLegs - 2)(b.head.get("target").parseDouble) ++ List(None, None)
    case Some(b) if b isArray => List.fill(nbLegs - 2 - b.size)(None) ++ b.map(_.get("target").parseDouble).toList ++ List(None, None)
    case _ => List.fill(nbLegs)(None)
  }

  def triggerList(formula:String, nbLegs:Int):List[List[String]] = formula.jsonNode match {
    case Some(b) if b.isArray && b.size == 1 => 
      List.fill(nbLegs - 2)(if (b.head isArray) b.head.map(_.parseString.getOrElse("")).toList else List.empty) ++ List.fill(2)(List.empty)
    case Some(b) if b isArray => 
      List.fill(nbLegs - b.size - 2)(List.empty) ++ b.map(n => if (n isArray) n.map(_.parseString.getOrElse("")).toList else List.empty) ++ List.fill(2)(List.empty)
    case _ => List.fill(nbLegs)(List.empty)
  }
    
	private def triggerMap(formula:List[List[String]], underlyings:List[String])(implicit fixingInfo:FixingInformation):List[Map[String, Double]] = {
	  formula.map(trig => 
	    (underlyings, trig).zipped.map{case (k, v) => (k, fixingInfo.updateCompute(v))}
	    .collect{case (k, Some(v)) => (k, v)}.toMap
	   )
	}
    
  def apply(
      formula:String, 
      underlyings:List[String], 
      legs:Int
    )(implicit fixingInfo:FixingInformation):Callabilities = {
    
    val bermudans = bermudan(formula, legs)
    val trigFormulas = triggerList(formula, legs)
    val trigMap = triggerMap(trigFormulas, underlyings)
    val targets = targetList(formula, legs)
    val calls = (bermudans.zip(trigFormulas)).zip(trigMap.zip(targets)).map{case ((berm, f), (trig, tgt)) => Callability(berm, trig, tgt, 0.0, (underlyings, f).zipped.toMap, None)}
	  Callabilities(calls)
  }
	  
	
	def apply(
    bermudans:List[Boolean], 
    triggers:List[List[Option[Double]]], 
    targets:List[Option[Double]],
    underlyings:List[String], 
    bonus:List[Double])(implicit fixingInfo:FixingInformation):Callabilities = {

    val trigmap = triggerMap(underlyings, triggers)
    Callabilities(bermudans.zip(trigmap).zip(bonus.zip(targets)).map{case ((berm, trig), (b, tgt)) => Callability(berm, trig, tgt, b, Map.empty, None)})
  }
	  
	def apply(
    bermudans:List[Boolean], 
    triggers:List[List[Option[Double]]], 
    targets:List[Option[Double]],
    underlyings:List[String])(implicit fixingInfo:FixingInformation):Callabilities = {
	  
    val trigmap = triggerMap(underlyings, triggers)
    Callabilities((bermudans, trigmap, targets).zipped.map{case (berm, trig, tgt) => Callability(berm, trig, tgt, 0.0, Map.empty, None)})
  }
	
	def triggerMap(underlyings:List[String], triggers:List[List[Option[Double]]]):List[Map[String, Double]] = {
	  triggers.map(trigs => {
      val t:Map[String, Double] = (underlyings, trigs).zipped.collect{case (k, Some(v)) => (k, v)}(collection.breakOut)
      t
    })
	}

}
