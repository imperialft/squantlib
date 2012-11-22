package squantlib.montecarlo.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper

/**
 * Interprets JSON formula specification for a linear formula with cap & floor.
 * JSON format:
 * - {type:"linear1d", description:XXX, variable:string, payoff:formula}, where
 *   formula = {min:double, max:double, mult:double, add:double}
 *   payment for array(i) is min <= mult * variable + add <= max
 */
case class Linear1dPayoff(val formula:String) extends Payoff {
  
	val mapper = new ObjectMapper
	val node = mapper.readTree(formula)
	val variable:String = node.get("variable").getTextValue
	val payoff = Linear1dFormula(node.get("payoff"))
  
	val variables:Set[String] = Set(variable)
	 
	override def price(fixings:List[Map[String, Double]]) = 
	  fixings.map(fixing => fixing match {
		  case f if f.contains(variable) => Some(payoff.price(f(variable)))
		  case _ => None
	})
	
	override def price(fixings:List[Double])(implicit d:DummyImplicit) = fixings.map(f => Some(payoff.price(f)))
	
	override def price = List(None)
	
	override def toString:String = payoff.toString
	
	override def legs = 1

}


case class Linear1dFormula (val subnode:JsonNode) {
  
	private def getvalue(name:String):Option[Double] = 
	  if (subnode has name) 
	    subnode.get(name) match {
	      case n if n.isNumber => Some(n.getDoubleValue)
	      case n if n.getTextValue.endsWith("%") => try {Some(n.getTextValue.dropRight(1).toDouble / 100)} catch { case _ => Some(Double.NaN)}
	      case _ => Some(Double.NaN)
	    }
	  else None
	
	val minValue:Option[Double] = getvalue("min")
	val maxValue:Option[Double] = getvalue("max")
	val coeff:Option[Double] = getvalue("mult")
	val constant:Option[Double] = getvalue("add")

	def price(fixing:Double):Double = {
	  var p = (coeff, constant) match {
	    case (None, None) => 0.0
		case (None, Some(c)) => c
		case (Some(x), None) => x * fixing
		case (Some(x), Some(c)) => x * fixing + c
	  }
	  
	  if (minValue.isDefined) p = p.max(minValue.get)
	  if (maxValue.isDefined) p = p.min(maxValue.get)
	  p
	}
	
}


