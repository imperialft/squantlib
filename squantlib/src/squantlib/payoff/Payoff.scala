package squantlib.payoff

import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import squantlib.database.fixings.Fixings
import org.jquantlib.time.{Date => qlDate}
import squantlib.model.Market

trait Payoff {
	
	/*
	 * List of reference variables. 
	 * Price fixings should be provided by map variable -> fixing value.
	 */	
	val variables:Set[String]
	
	/*
	 * Price assuming all variables fixed at spot market.
	 */	
	def price(market:Market):Double = price(market.getFixings(variables))
	
	/*
	 * Price given variable fixings, where set of necessary variables are provided by variables function.
	 */	
	def price(fixings:Map[String, Double]):Double
	
	/*
	 * Returns price if there's only one variable. Returns NaN in case of >1 variables.
	 */	
	def price(fixing:Double):Double 
	
	/*
	 * Returns price if there's no variable. Returns NaN in case of >0 variables.
	 */	
	def price:Double
	
	/*
	 * Price in case of multiple event dates.
	 * Only refers to the last variable by default but can be overridden.
	 */	
	def price(fixings:List[Map[String, Double]]):Double = if (fixings.isEmpty) price else price(fixings.last)
	
	/*
	 * Price in case of multiple event dates with only one variable.
	 * Only refers to the last variable by default but can be overridden.
	 */	
	def price[T:ClassManifest](fixings:List[Double]):Double = if (fixings.isEmpty) price else price(fixings.last)
	

	/*
	 * Event dates, usually used in case of >1 fixing dates.
	 */	
	def eventDates(d:CalculationPeriod):List[qlDate] = List(d.eventDate)
	
	/*
	 * Returns FixedPayoff if all variables fixing are available on given date. 
	 * Returns this object if any variables are missing.
	 */	
	def applyFixing(eventDate:qlDate):Payoff = 
	  if (variables.size == 0) this
	  else {
	    val fixings:Map[String, Double] = variables.map(v => Fixings.byDate(v, eventDate).collect{case (_, f) => (v, f)}).flatMap(x => x) (collection.breakOut)
	    applyFixing(fixings)
	  }
	
	/*
	 * Returns FixedPayoff if all variables fixing are available on given market. 
	 * Returns this object if any variables are missing.
	 */	
	def applyFixing(market:Market):Payoff = applyFixing(market.getFixings(variables))
	
	/*
	 * Returns FixedPayoff if all fixings are provided by the mapping.
	 * Returns this object if any variables are missing.
	 */	
	def applyFixing(fixings:Map[String, Double]):Payoff = fixings match {
	  case f if (f.isEmpty || variables.size == 0) => this
	  case f if variables subsetOf fixings.keySet => {
	    val comment = "{" + fixings.map{case (variable, v) => "\"" + variable + "\":" + v}.mkString(", ") + "}"
	    FixedPayoff(price(fixings), comment)
	  }
	  case _ => this
	}
	
	/*
	 * Returns FixedPayoff if there is <= 1 variable and the value is provided. 
	 * Returns this object otherwise.
	 */	
	def applyFixing(fixing:Double):Payoff = if (variables.size == 1) FixedPayoff(fixing, "{ \"ref\":" + fixing + "}") else this
	
	def description:String
	
	def display(isRedemption:Boolean):String
	
	def jsonString:String
	
	def toString:String
}
 
object Payoff {
  
	def apply(formula:String):Option[Payoff] =
	  if (formula == null || formula.trim.isEmpty) None
	  else Some(payoffType(formula) match {
	    case "fixed" => FixedPayoff(formula)
		case "leps1d" => LEPS1dPayoff(formula)
		case "linear1d" => Linear1dPayoff(formula)
		case "putdi" => PutDIPayoff(formula)
		case "putdiamerican" => PutDIAmericanPayoff(formula)
		case "forward" => ForwardPayoff(formula)
		case "null" => NullPayoff(formula)
	    case "binary" => BinaryPayoff(formula)
	    case "general" => GeneralPayoff(formula)
		case _ => GeneralPayoff(formula)
	  })
  
	def payoffType(formula:String):String = formula match {
	  case f if f.parseDouble.isDefined => "fixed"
	  case f if f.startsWith("leps") => "leps1d"
	  case f => formula.parseJsonString("type").orNull
	  }
}

