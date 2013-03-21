package squantlib.payoff

import scala.collection.LinearSeq
import scala.annotation.tailrec 

case class Callability(bermudan:Boolean, triggers:List[Option[Double]], variables:List[String], bonus:Double) {
  
  def isBermuda:Boolean = bermudan
  
  def isTrigger:Boolean = !triggers.isEmpty && triggers.exists(_.isDefined)
  
  def isEmpty:Boolean = !bermudan && (triggers.isEmpty || triggers.forall(_.isEmpty))
  
  override def toString:String = 
    (if (bermudan) "call " else "") + 
    (if (isTrigger) "trig:" + triggers.mkString(" ") else "") + 
    (if (bonus != 0.0) " bonus:" + bonus else "")
}

object Callability {
  
  val empty = Callability(false, List.empty, List.empty, 0.0)
}

case class Callabilities(calls:List[Callability]) extends LinearSeq[Callability] {
  
	val variables:Set[String] = calls.map(_.variables).flatten.toSet
	
	val bermudans:List[Boolean] = calls.map(_.bermudan)
	
	val triggers:List[List[Option[Double]]] = calls.map(_.triggers)
	
	val triggerMap:List[Option[Map[String, Double]]] = calls.map(t => 
	  (t.variables, t.triggers).zipped.collect{case (k, Some(v)) => (k, v)})
	  .map(t => if (t.isEmpty) None else Some(t.toMap))
	
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
    
	override def isEmpty:Boolean = calls.isEmpty || calls.forall(_.isEmpty)
	
	override def head:Callability = calls.head
	
	override def tail = calls.tail
	
	override def length = calls.length
	
	override def iterator:Iterator[Callability] = calls.iterator
	
	override def toList:List[Callability] = calls
	
	def reorder(order:List[Int]) = new Callabilities((0 to calls.size-1).toList.map(i => calls(order(i))))
	
}


object Callabilities {
	
	def empty:Callabilities = new Callabilities(List.empty)
	
	def apply(calls:LinearSeq[Callability]):Callabilities = new Callabilities(calls.toList)
	
	def apply(bermudans:List[Boolean], triggers:List[List[Option[Double]]], variables:List[String], bonus:List[Double]):Callabilities = 
	  Callabilities((bermudans, triggers, bonus).zipped.map{case (b, t, n) => Callability(b, t, variables, n)})
	  
	def apply(bermudans:List[Boolean], triggers:List[List[Option[Double]]], variables:List[String]):Callabilities = 
	  Callabilities((bermudans, triggers).zipped.map{case (b, t) => Callability(b, t, variables, 0.0)})
	
}
