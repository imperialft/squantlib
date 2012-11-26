package squantlib.montecarlo.payoff

import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper
import scala.collection.LinearSeq
import scala.annotation.tailrec 
import DisplayUtils._
import JsonUtils._

case class Payoffs(val payoffs:List[Payoff]) {
 
	val variables = {
	  @tailrec def variablesRec(paylist:List[Payoff], acc:Set[String]):Set[String] = {
		if (paylist.isEmpty) acc
		else variablesRec(paylist.tail, paylist.head.variables ++ acc)
	  }
	  variablesRec(payoffs, Set.empty)
	}
	
	def size:Int = payoffs.size
	
	val factors:Int = variables.size
	
	def price(fixings:List[Map[String, Double]]):List[Option[Double]] = {
	  assert(fixings.size == this.size)
	  
	  @tailrec def priceRec(paylist:List[Payoff], fixlist:List[Map[String, Double]], acc:List[Option[Double]]):List[Option[Double]] = {
	    if (paylist.isEmpty) acc
	    else priceRec(paylist.tail, fixlist.tail, acc :+ paylist.head.price(fixlist.head))
	  }
	  
	  priceRec(payoffs, fixings, List.empty)
	}
	
	def price(fixings:List[Double]) (implicit d:DummyImplicit):List[Option[Double]] = {
	  assert(fixings.size == this.size && factors <= 1)
	  
	  @tailrec def priceRec(paylist:List[Payoff], fixlist:List[Double], acc:List[Option[Double]]):List[Option[Double]] = {
	    if (paylist.isEmpty) acc
	    else priceRec(paylist.tail, fixlist.tail, acc :+ paylist.head.price(fixlist.head))
	  }
	  
	  priceRec(payoffs, fixings, List.empty)
	}
	  
	def price:List[Option[Double]] = {
	  assert(factors == 0)
	  payoffs.map(_.price)
	}
	
	
	def ++(another:Payoffs) = new Payoffs(payoffs ++ another.payoffs)
	def :+(payoff:Payoff) = new Payoffs(payoffs :+ payoff)
	def head = payoffs.head
	def tail = new Payoffs(payoffs.tail)
	
	override def toString = payoffs.map(_.toString).mkString("\n")
	
}


object Payoffs {
  
	def apply(formula:String):Payoffs =	{
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
	   println("initialize " + payofflist.size + " payoffs")
	   new Payoffs(payofflist)
	}
	
	def payoffType(formula:String):String = formula.trim match {
	  case f if f.parseDouble.isDefined => "fixed"
	  case f if f.startsWith("leps") => "leps1d"
	  case f => formula.parseJsonString("type")
	  }
	  
}
