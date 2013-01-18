package squantlib.payoff

import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import scala.collection.LinearSeq
import scala.annotation.tailrec 
import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._

case class Payoffs(payoffs:List[Payoff]) extends LinearSeq[Payoff]{
  
	val variables:Set[String] = {
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
	
	def price(fixings:List[Map[String, Double]], trigger:List[Map[String, Double]], trigAmount:List[Double]):List[Double] = {
	  assert(fixings.size == this.size && fixings.size == trigger.size)
	  assert(trigger.head.keySet subsetOf fixings.head.keySet)
	  
	  @tailrec def priceRec(paylist:List[Payoff], fixlist:List[Map[String, Double]], acc:List[Double], triglist:List[Map[String, Double]], trigamt:List[Double], triggered:Boolean):List[Double] = {
	    if (paylist.isEmpty) acc
	    else if (triggered) priceRec(paylist.tail, fixlist.tail, acc :+ 0.0, triglist.tail, trigamt.tail, true)
	    else if (triglist.head.forall{case (v, d) => d > fixlist.head(v)})
	      priceRec(paylist.tail, fixlist.tail, acc :+ paylist.head.price(fixlist.head), triglist.tail, trigamt.tail, false)
	    else priceRec(paylist.tail, fixlist.tail, acc :+ (paylist.head.price(fixlist.head) + trigamt.head), triglist.tail, trigamt, true)
	  }
	  
	  priceRec(payoffs, fixings, List.empty, trigger, trigAmount, false)
	}
	
	def price(fixings:List[Double]) (implicit d:DummyImplicit):List[Double] = {
	  assert(fixings.size == this.size && factors <= 1)
	  
	  @tailrec def priceRec(paylist:List[Payoff], fixlist:List[Double], acc:List[Double]):List[Double] = {
	    if (paylist.isEmpty) acc
	    else priceRec(paylist.tail, fixlist.tail, acc :+ paylist.head.price(fixlist.head))
	  }
	  
	  priceRec(payoffs, fixings, List.empty)
	}
	
	def price(fixings:List[Double], trigger:List[Option[Double]], trigAmount:List[Double])(implicit d:DummyImplicit):List[Double] = {
	  assert(fixings.size == this.size && fixings.size == trigger.size)
	  
	  @tailrec def priceRec(paylist:List[Payoff], fixlist:List[Double], acc:List[Double], triglist:List[Option[Double]], trigamt:List[Double], triggered:Boolean):List[Double] = {
	    if (paylist.isEmpty) acc
	    else if (triggered) priceRec(paylist.tail, fixlist.tail, acc :+ 0.0, triglist.tail, trigamt.tail, true)
	    else if (triglist.head.isEmpty || triglist.head.get > fixlist.head)
	      priceRec(paylist.tail, fixlist.tail, acc :+ paylist.head.price(fixlist.head), triglist.tail, trigamt.tail, false)
	    else priceRec(paylist.tail, fixlist.tail, acc :+ (paylist.head.price(fixlist.head) + trigamt.head), triglist.tail, trigamt.tail, true)
	  }
	  
	  priceRec(payoffs, fixings, List.empty, trigger, trigAmount, false)
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
	
	val jsonString:String = payoffs.map(_.jsonString).mkString(";")
}

import scala.collection.JavaConversions._

object Payoffs {
	
	def empty:Payoffs = new Payoffs(List.empty)
	
	def apply(formula:String, legs:Int = 0):Payoffs =	{
	  
	  val payofflist:List[Payoff] = formula.jsonNode match {
	    case Some(n) if n isArray => n.getElements.toList.map(f => getPayoff(toJsonString(f))).flatten
	    case _ => formula.split(";").toList.map(getPayoff).flatten
	  }
	  
	  val fullpayoff = if (payofflist.size < legs) List.fill(legs - payofflist.size)(payofflist.head) ++ payofflist else payofflist
	  Payoffs(fullpayoff)
	}
	
	def toJsonString(n:JsonNode):String = (new ObjectMapper).writeValueAsString(n)
	
	def payoffType(formula:String):String = formula.trim match {
	  case f if f.parseDouble.isDefined => "fixed"
	  case f if f.startsWith("leps") => "leps1d"
	  case f => formula.parseJsonString("type")
	  }
	
	def getPayoff(f:String):List[Payoff] = payoffType(f) match {
	      case "fixed" => List(FixedPayoff(f))
		  case "fixedseries" => FixedPayoffSeries(f)
		  case "leps1d" => List(LEPS1dPayoff(f))
		  case "leps1dseries" => LEPS1dPayoffSeries(f) 
		  case "linear1d" => List(Linear1dPayoff(f))
		  case "linear1dseries" => Linear1dPayoffSeries(f)
		  case "putdi" => List(PutDIPayoff(f))
		  case "forward" => List(ForwardPayoff(f))
		  case "null" => List(NullPayoff(f))
		  case "binary" => List(BinaryPayoff(f))
		  case "general" => List(GeneralPayoff(f))
		  case _ => List(GeneralPayoff(f))
		}

	  
}
