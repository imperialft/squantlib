package squantlib.montecarlo.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper

/**
 * Interprets JSON formula specification for a fixed leg.
 * JSON format: {type:"fixed", description:XXX, payoff:double}
 * Natual format: 0.035 or "3.5%"
 */
case class FixedPayoff(val formula:String) extends Payoff {
  
	val payoff:Option[Double] = FixedPayoff.stringToDouble(formula) orElse FixedPayoff.getJsonValue(formula, "payoff")
  
	val variables:Set[String] = Set.empty
	 
	override def price(fixings:List[Map[String, Double]]) = List.fill(fixings.size)(payoff)
	override def price(fixing:List[Double])(implicit d:DummyImplicit) = List.fill(fixing.size)(payoff)
	override def price = List(payoff)
	
	override def toString:String = payoff.toString
	
	override def legs = 1
	
}


object FixedPayoff {
  
	def stringToDouble(f:String):Option[Double] = f match {
	  case n if n.isEmpty => None
	  case n if n.endsWith("%") => try {Some(n.dropRight(1).toDouble / 100)} catch { case _ => None}
	  case n => try {Some(n.toDouble)} catch { case _ => None}
	}
	
	def getJsonValue(formula:String, name:String):Option[Double] = {
	  val mapper = new ObjectMapper
	  val node = mapper.readTree(formula)
	  
	  if (node has name) 
	    node get name match {
	      case n if n isNumber => Some(n.getDoubleValue)
	      case n if n.getTextValue endsWith("%") => 
	        try {Some(n.getTextValue.dropRight(1).toDouble / 100)} 
	        catch { case e:Exception => println(e.getMessage) ; Some(Double.NaN)}
	      case _ => Some(Double.NaN)
	    }
	  else None
	}
}
