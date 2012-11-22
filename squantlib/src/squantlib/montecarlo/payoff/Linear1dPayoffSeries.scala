package squantlib.montecarlo.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper


/**
 * Interprets JSON formula for series of linear formulas with caps and floors.
 * JSON format:
 * - {type:"linear1dseries", variable:string, payoff:Array[formula]}, where
 *   formula = {min:double, max:double, mult:double, add:double}
 *   payment for array(i) is min <= mult * variable + add <= max
 */
case class Linear1dPayoffSeries(val formula:String) extends Payoff {
  
	val mapper = new ObjectMapper
	val node = mapper.readTree(formula)
	val variable:String = node.get("variable").getTextValue
	val payoffs = node.get("payoff").getElements.map(Linear1dFormula).toList
	val paycount = payoffs.size
  
	val variables:Set[String] = Set(variable)
	
	override def price(fixings:List[Double])(implicit d:DummyImplicit):List[Option[Double]] = {
	  assert(fixings.size == paycount)
	  if (payoffs.isEmpty) List.empty
	  else (for (i <- 0 to paycount - 1) yield (Some(payoffs(i).price(fixings(i))))).toList
	}
	
	override def price(fixings:List[Map[String, Double]]):List[Option[Double]] = {
	  assert(fixings.size == paycount && fixings.forall(_.contains(variable)))
	  price(fixings.map(_(variable)))
	}
	
	override def price:List[Option[Double]] = List.empty
	 
	override def toString:String = payoffs.toString
	
	override def legs = payoffs.size
}


