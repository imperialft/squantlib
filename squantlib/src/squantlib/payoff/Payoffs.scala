package squantlib.payoff

import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import scala.collection.LinearSeq
import scala.annotation.tailrec 
import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._

case class Payoffs(val payoffs:List[Payoff]) extends LinearSeq[Payoff]{
 
	val variables = {
	  @tailrec def variablesRec(paylist:List[Payoff], acc:Set[String]):Set[String] = {
		if (paylist.isEmpty) acc
		else variablesRec(paylist.tail, paylist.head.variables ++ acc)
	  }
	  variablesRec(payoffs, Set.empty)
	}
	
	val factors:Int = variables.size
	
	def price(fixings:List[Map[String, Double]]):List[Double] = {
	  assert(fixings.size == this.size)
	  
	  @tailrec def priceRec(paylist:List[Payoff], fixlist:List[Map[String, Double]], acc:List[Double]):List[Double] = {
	    if (paylist.isEmpty) acc
	    else priceRec(paylist.tail, fixlist.tail, acc :+ paylist.head.price(fixlist.head))
	  }
	  
	  priceRec(payoffs, fixings, List.empty)
	}
	
	def price(fixings:List[Double]) (implicit d:DummyImplicit):List[Double] = {
	  assert(fixings.size == this.size && factors <= 1)
	  
	  @tailrec def priceRec(paylist:List[Payoff], fixlist:List[Double], acc:List[Double]):List[Double] = {
	    if (paylist.isEmpty) acc
	    else priceRec(paylist.tail, fixlist.tail, acc :+ paylist.head.price(fixlist.head))
	  }
	  
	  priceRec(payoffs, fixings, List.empty)
	}
	  
	def price:List[Double] = {
	  assert(factors == 0)
	  payoffs.map(_.price)
	}
	
	/* Replaces already-fixed payoffs to fixed leg
	 */
	
	def applyFixing(fixings:List[Map[String, Double]]):Payoffs = {
	  assert(fixings.size == this.size)
	  
	  @tailrec def fixingRec(paylist:List[Payoff], fixlist:List[Map[String, Double]], acc:List[Payoff]):List[Payoff] = {
	    if (paylist.isEmpty) acc
	    else fixingRec(paylist.tail, fixlist.tail, acc :+ paylist.head.applyFixing(fixlist.head))
	  }
	  
	  Payoffs(fixingRec(payoffs, fixings, List.empty))
	}
	
	def applyFixing(fixings:List[Option[Double]]) (implicit d:DummyImplicit):Payoffs = {
	  assert(fixings.size == this.size && factors <= 1)
	  
	  @tailrec def fixingRec(paylist:List[Payoff], fixlist:List[Option[Double]], acc:List[Payoff]):List[Payoff] = {
	    if (paylist.isEmpty) acc
	    else fixingRec(paylist.tail, fixlist.tail, acc :+ paylist.head.applyFixing(fixlist.head))
	  }
	  
	  Payoffs(fixingRec(payoffs, fixings, List.empty))
	}
	
	def ++(another:Payoffs) = new Payoffs(payoffs ++ another.payoffs)
	def :+(payoff:Payoff) = new Payoffs(payoffs :+ payoff)
	
	override def toString = payoffs.map(_.toString).mkString("\n")
	
    def apply(i:Int):Payoff = payoffs(i)
	override def isEmpty:Boolean = payoffs.isEmpty
	override def head:Payoff = payoffs.head
	override def tail = payoffs.tail
	override def length = payoffs.length
	override def iterator:Iterator[Payoff] = payoffs.iterator
	
	override def toList:List[Payoff] = payoffs
	
	def reorder(order:List[Int]) = new Payoffs((0 to payoffs.size-1).toList.map(i => payoffs(order(i))))
	
}


object Payoffs {
	
	def empty:Payoffs = new Payoffs(List.empty)
	
	def apply(formula:String, legs:Int = 0):Payoffs =	{
	  
	  val payofflist:List[Payoff] = formula.split(";").toList.map{f => 
	    payoffType(f) match {
	      case "fixed" => List(FixedPayoff(f))
		  case "fixedseries" => FixedPayoffSeries(f)
		  case "leps1d" => List(LEPS1dPayoff(f))
		  case "leps1dseries" => LEPS1dPayoffSeries(f) 
		  case "linear1d" => List(Linear1dPayoff(f))
		  case "linear1dseries" => Linear1dPayoffSeries(f)
		  case _ => List(GeneralPayoff(f).remodelize)
		}
	   }.flatten
	   
	   val fullpayoff = if (payofflist.size < legs) List.fill(legs - payofflist.size)(payofflist.head) ++ payofflist else payofflist

	   println("initialize " + fullpayoff.size + " payoffs (" + payofflist.size + " input legs)")
	   new Payoffs(fullpayoff)
	}
	
	def payoffType(formula:String):String = formula.trim match {
	  case f if f.parseDouble.isDefined => "fixed"
	  case f if f.startsWith("leps") => "leps1d"
	  case f => formula.parseJsonString("type")
	  }
	  
}
