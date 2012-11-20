package squantlib.montecarlo.payoff

import org.jquantlib.time.{Date => qlDate}

trait Payoff{
  
	val variables:Set[String]
	def factors:Int = variables.size
	
	def price(fixings:Map[String, Double]):Option[Double]
	
	def price(fixing:Double):Option[Double] = factors match {
	  case 0 => price
	  case 1 => price(Map(variables.head -> fixing))
	  case f => None
	}
	
	def price():Option[Double] = factors match {
	  case 0 => price(Map("" -> 0.0))
	  case s => None
	}
	
}

trait PayoffSeries{
	
	val variables:Set[String]
	def factors:Int = variables.size
	
	def price(fixings:List[Double]) (implicit d:DummyImplicit):List[Option[Double]] 
	
	def price(fixings:List[Map[String, Double]]):List[Option[Double]]
	
	def price():List[Option[Double]]
	
}

