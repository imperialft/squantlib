package squantlib.montecarlo.payoff

import squantlib.setting.initializer.Currencies
import scala.collection.mutable.{Map => mutableMap}
import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper

/**
 * Interprets JSON formula specification for a fixed leg.
 * JSON format:
 * - {type:"fixed", description:XXX, payoff:double}
 */
class FixedPayoff(val formula:String) extends Payoff {
  
	val mapper = new ObjectMapper
	val node = mapper.readTree(formula)
	val payoff:Option[Double] = getvalue("payoff")
  
	val variables:Set[String] = Set.empty
	 
	override def price(fixings:Map[String, Double]):Option[Double] = payoff
	override def price(fixing:Double):Option[Double] = payoff
	override def toString:String = payoff.toString
	
	private def getvalue(name:String):Option[Double] = 
	  if (node has name) 
	    node.get(name) match {
	      case n if n.isNumber => Some(n.getDoubleValue)
	      case n if n.getTextValue.endsWith("%") => 
	        try {Some(n.getTextValue.dropRight(1).toDouble / 100)} 
	        catch { case e:Exception => println(e.getMessage) ; Some(Double.NaN)}
	      case _ => Some(Double.NaN)
	    }
	  else None
}


/**
 * Interprets JSON formula for series of linear formulas with caps and floors.
 * JSON format:
 * - {type:"fixedseries", description:XXX, payoff:Array[double]}
 */
class FixedPayoffSeries(val formula:String) extends PayoffSeries {
  
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
	 
	override def toString:String = payoffs.toString
	
	private def getvalue(node:JsonNode):Option[Double] = 
	  node match {
	  	case n if n.isNumber => Some(n.getDoubleValue)
	    case n if n.getTextValue.endsWith("%") => 
	      try {Some(n.getTextValue.dropRight(1).toDouble / 100)} 
	      catch { case e:Exception => println(e.getMessage) ; Some(Double.NaN)}
	    case _ => Some(Double.NaN)
	}
	
}



