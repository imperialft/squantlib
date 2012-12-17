package squantlib.payoff

import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import squantlib.database.fixings.Fixings
import org.jquantlib.time.{Date => qlDate}
import squantlib.model.Market

trait Payoff{
	
	val variables:Set[String]
	
	def price(fixings:Map[String, Double]):Double
	
	def price(fixing:Double) (implicit d:DummyImplicit):Double 
	
	def price:Double
	
	def toString:String
	
	def applyFixing(eventDate:qlDate):Payoff = 
	  if (variables.size == 0) this
	  else {
	    val fixings = variables.map(v => Fixings.byDate(v, eventDate).collect{case (d, f) => (v, f)}).flatMap(x => x).toMap
	    applyFixing(fixings)
	  }
	
	def applyFixing(fixings:Map[String, Double]):Payoff = fixings match {
	  case f if (f.isEmpty || variables.size == 0) => this
	  case f if variables.forall(fixings.contains) => {
	    val comment = "{" + fixings.map{case (variable, v) => "\"" + variable + "\":" + v}.mkString(", ") + "}"
	    FixedPayoff(price(fixings), comment)
	  }
	  case _ => this
	}
	
	def applyFixing(fixing:Option[Double]):Payoff = fixing match {
	  case None => this
	  case Some(f) if variables.size == 1 => FixedPayoff(f, "{ \"ref\":" + fixing + "}")
	  case _ => this
	}
	
	def spotCoupon(mkt:Market):Double = price(mkt.getFixings(variables)) 
	
	val description:String
}

object Payoff {
  
	def apply(formula:String):Payoff =
	  payoffType(formula) match {
	    case "fixed" => FixedPayoff(formula)
		case "leps1d" => LEPS1dPayoff(formula)
		case "linear1d" => Linear1dPayoff(formula)
		case _ => GeneralPayoff(formula).remodelize
	  }
  
	def payoffType(formula:String):String = formula match {
	  case f if f.parseDouble.isDefined => "fixed"
	  case f if f.startsWith("leps") => "leps1d"
	  case f => formula.parseJsonString("type")
	  }
}

