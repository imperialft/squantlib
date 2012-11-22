package squantlib.montecarlo.payoff

import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.map.ObjectMapper


trait Payoff{
	
	val variables:Set[String]
	
	def factors:Int = variables.size
	
	def price(fixings:List[Map[String, Double]]):List[Option[Double]]
	
	def price(fixings:List[Double]) (implicit d:DummyImplicit):List[Option[Double]] 
	
	def price:List[Option[Double]]
	
	def price(fixing:Double):Option[Double] = price(List(fixing)).head
	
	def price(fixing:Map[String, Double]):Option[Double] = price(List(fixing)).head
	
	def legs:Int
	
}


object Payoff {
	
	def apply(formula:String):Payoff = payoffType(formula) match {
	  case "fixed" => new FixedPayoff(formula)
	  case "fixedseries" => new FixedPayoffSeries(formula)
	  case "leps1d" => new LEPS1dPayoff(formula)
	  case "leps1dseries" => new LEPS1dPayoffSeries(formula)
	  case "linear1d" => new Linear1dPayoff(formula)
	  case "linear1dseries" => new Linear1dPayoffSeries(formula)
	  case _ => new GeneralPayoff(formula)
	}
	
	def payoffType(formula:String):String = 
	  try { 
	    (new ObjectMapper).readTree(formula) match {
		    case node if node.isObject => node get("type") getTextValue
		    case node if node.isArray => "fixedseries"
		    case _ => null
	    }}
	
	  catch { 
	    case _ => FixedPayoff.stringToDouble(formula) match {
		    case Some(e) => "fixed"
		    case None => null
	    }}
	  
}