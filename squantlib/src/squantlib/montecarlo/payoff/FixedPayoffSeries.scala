package squantlib.montecarlo.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper


/**
 * Interprets JSON formula for series of linear formulas with caps and floors.
 * JSON format:
 * - {type:"fixedseries", description:XXX, payoff:Array[double]}
 */
case class FixedPayoffSeries(val formula:String) extends Payoff {
  
	val mapper = new ObjectMapper
	val node = mapper.readTree(formula)
	val payoffs:List[Option[Double]] = node.get("payoff").getElements.map(getvalue).toList
	val paycount = payoffs.size
  
	val variables:Set[String] = Set.empty
	
	override def price(fixings:List[Double])(implicit d:DummyImplicit):List[Option[Double]] = {
	  assert(fixings.size == paycount)
	  payoffs
	}
	
	override def price(fixings:List[Map[String, Double]]):List[Option[Double]] = {
	  assert(fixings.size == paycount)
	  payoffs
	}
	
	override def price:List[Option[Double]] = payoffs
	 
	private def getvalue(node:JsonNode):Option[Double] = 
	  node match {
	  	case n if n.isNumber => Some(n.getDoubleValue)
	    case n if n.getTextValue.endsWith("%") => 
	      try {Some(n.getTextValue.dropRight(1).toDouble / 100)} 
	      catch { case e:Exception => println(e.getMessage) ; Some(Double.NaN)}
	    case _ => Some(Double.NaN)
	}
	
	override def toString:String = payoffs.toString
	
	override def legs = payoffs.size
}



