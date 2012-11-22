package squantlib.montecarlo.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper


/**
 * Interprets JSON formula for series of LEPS1dPayoffs.
 * JSON format:
 * - {type:"leps1dseries", variable:string, payoff:Array[formula]}, where
 *   formula = Array[{minrange:double, maxrange:double, mult:double, add:double}]
 *   payment for array(i) is 
 *     if minrange(i) <= X < maxrange(i) => mult(i) * variable + add(i)
 *     otherwise zero
 */
case class LEPS1dPayoffSeries(val formula:String) extends Payoff {
  
	val mapper = new ObjectMapper
	val node = mapper.readTree(formula)
	val variable:String = node.get("variable").getTextValue
	val payoffs:List[LEPS1dFormula] = node.get("payoff").getElements.map(LEPS1dFormula).toList
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

