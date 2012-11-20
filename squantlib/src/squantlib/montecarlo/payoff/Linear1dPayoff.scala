package squantlib.montecarlo.payoff

import squantlib.setting.initializer.Currencies
import scala.collection.mutable.{Map => mutableMap}
import scala.collection.JavaConversions._
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper

/**
 * Interprets JSON formula specification for a linear formula with cap & floor.
 * JSON format:
 * - {type:"1dlinear", description:XXX, variable:string, payoff:formula}, where
 *   formula = {min:double, max:double, mult:double, add:double}
 *   payment for array(i) is min <= mult * variable + add <= max
 */
class Linear1dPayoff(val formula:String) extends Payoff {
  
	val mapper = new ObjectMapper
	val node = mapper.readTree(formula)
	val variable:String = node.get("variable").getTextValue
	val Paymentformula = Linear1dFormula(node.get("payoff"))
  
	val variables:Set[String] = Set(variable)
	 
	override def price(fixings:Map[String, Double]):Option[Double] = 
	  if (fixings.contains(variable)) price(fixings(variable))
	  else None
	
	override def price(fixing:Double):Option[Double] = Some(Paymentformula.price(fixing))
	
	override def toString:String = Paymentformula.toString
	
}


/**
 * Interprets JSON formula for series of linear formulas with caps and floors.
 * JSON format:
 * - {type:"1dlinearseries", variable:string, payoff:Array[formula]}, where
 *   formula = {min:double, max:double, mult:double, add:double}
 *   payment for array(i) is min <= mult * variable + add <= max
 */
class Linear1dPayoffSeries(val formula:String) extends PayoffSeries {
  
	val mapper = new ObjectMapper
	val node = mapper.readTree(formula)
	val variable:String = node.get("variable").getTextValue
	val payoffs:List[Linear1dFormula] = node.get("payoff").getElements.map(Linear1dFormula).toList
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


