package squantlib.payoff

import scala.collection.LinearSeq
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.breakOut
import squantlib.util.DisplayUtils._

case class Callabilities(calls:List[Callability]) extends LinearSeq[Callability] with FixingLegs[Callability]{
  
	val underlyings:Set[String] = calls.map(_.underlyings).flatten.toSet
	
	def bermudans:List[Boolean] = calls.map(_.bermudan)
	
	def triggers:List[Option[Map[String, Double]]] = calls.map(c => if (c isTrigger) Some(c.triggers) else None)
	
	def triggerValues(variables:List[String]):List[List[Option[Double]]] = calls.map(_.triggerValues(variables))
	
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
    
    def isTrigger = calls.exists(_.isTrigger)
    
	def triggerCheck(fixings:List[Map[String, Double]]):List[Boolean] = (fixings, calls).zipped.map{case (f, c) => c.isTriggered(f)}
	
	override def isEmpty:Boolean = calls.isEmpty || calls.forall(_.isEmpty)
	
	override def head:Callability = calls.head
	
	override def tail = calls.tail
	
	override def length = calls.length
	
	override def iterator:Iterator[Callability] = calls.iterator
	
	override def toList:List[Callability] = calls
	
	def reorder(order:List[Int]) = new Callabilities((0 to calls.size-1).map(i => calls(order(i))) (collection.breakOut)) 
	
	override val fixinglegs = calls
//	def assignFixings(fixings:List[Map[String, Double]]):Unit = {
//	  assert(fixings.size == calls.size)
//	  (calls, fixings).zipped.foreach{case (c, f) => c.assignFixings(f)}
//	}
//	
//	def assignFixings(fixings:List[Option[Double]]) (implicit d:DummyImplicit):Unit = {
//	  assert(fixings.size == calls.size)
//	  (calls, fixings).zipped.foreach{
//	    case (c, Some(f)) => c.assignFixings(f)
//	    case (c, None) => {}}
//	}
	
}


object Callabilities {
	
	def empty:Callabilities = new Callabilities(List.empty)
	
	def apply(calls:LinearSeq[Callability]):Callabilities = new Callabilities(calls.toList)
	
	def apply(bermudans:List[Boolean], triggers:List[List[Option[Double]]], underlyings:List[String], bonus:List[Double]):Callabilities = {
	  val trigmap = triggers.map(trigs => {
	    val t:Map[String, Double] = (underlyings, trigs).zipped.collect{case (k, Some(v)) => (k, v)}(breakOut); t})
	  Callabilities((bermudans, trigmap, bonus).zipped.map{case (b, t, n) => Callability(b, t, n)}
	  )
	}
	  
	def apply(bermudans:List[Boolean], triggers:List[List[Option[Double]]], underlyings:List[String]):Callabilities = {
	  val trigmap = triggers.map(trigs => {
	    val t:Map[String, Double] = (underlyings, trigs).zipped.collect{case (k, Some(v)) => (k, v)}(breakOut); t})
	  Callabilities((bermudans, trigmap).zipped.map{case (b, t) => Callability(b, t, 0.0)})
	}
	
}
