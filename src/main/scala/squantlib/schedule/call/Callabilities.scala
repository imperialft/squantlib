package squantlib.schedule.call

import scala.language.postfixOps
import scala.collection.LinearSeq
import squantlib.util.DisplayUtils._
import squantlib.schedule.FixingLegs
import squantlib.util.FixingInformation

case class Callabilities(calls:List[Callability]) extends LinearSeq[Callability] with FixingLegs[Callability]{
  
	val underlyings:Set[String] = calls.map(_.underlyings).flatten.toSet
	
	def bermudans:List[Boolean] = calls.map(_.bermudan)
	
	def triggers:List[Option[Map[String, Double]]] = calls.map(c => 
	  if (c.isTriggered) Some(c.triggers.map{case (k, v) => (k, 0.0)})
	  else if (c.isFixed) None
	  else if (c isTrigger) Some(c.triggers) 
	  else None)
	
	def triggerValues(variables:List[String]):List[List[Option[Double]]] = calls.map(_.triggerValues(variables))
	
	def isTriggered:Boolean = calls.exists(c => c.isFixed && c.fixedTrigger == Some(true))
  
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
    
    val isTrigger = calls.exists(_.isTrigger) && !isTriggered
    
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
	
}


object Callabilities {
	
	def empty:Callabilities = new Callabilities(List.empty)
	
	def apply(calls:LinearSeq[Callability]):Callabilities = new Callabilities(calls.toList)
	
	def apply(
	    bermudans:List[Boolean], 
	    triggers:List[List[Option[Double]]], 
	    underlyings:List[String], 
	    bonus:List[Double])(implicit fixingInfo:FixingInformation):Callabilities = {
	  val trigmap = triggers.map(trigs => {
	    val t:Map[String, Double] = (underlyings, trigs).zipped.collect{case (k, Some(v)) => (k, v)}(collection.breakOut); t})
	  Callabilities((bermudans, trigmap, bonus).zipped.map{case (b, t, n) => Callability(b, t, n, "")}
	  )
	}
	  
	def apply(
	    bermudans:List[Boolean], 
	    triggers:List[List[Option[Double]]], 
	    underlyings:List[String])(implicit fixingInfo:FixingInformation):Callabilities = {
	  val trigmap = triggers.map(trigs => {
	    val t:Map[String, Double] = (underlyings, trigs).zipped.collect{case (k, Some(v)) => (k, v)}(collection.breakOut); t})
	  Callabilities((bermudans, trigmap).zipped.map{case (b, t) => Callability(b, t, 0.0, "")})
	}
	
}
