package squantlib.schedule.payoff

import squantlib.util.DisplayUtils._
import squantlib.util.JsonUtils._
import squantlib.database.DB
import squantlib.util.Date
import squantlib.model.market.Market
import scala.Option.option2Iterable
import squantlib.schedule.CalculationPeriod
import squantlib.schedule.FixingLeg
import scala.reflect.ClassTag

trait Payoff extends FixingLeg {
	
	/*
	 * List of reference variables. 
	 * Price fixings should be provided by map variable -> fixing value.
	 */	
	override val variables:Set[String]
	
	val isPriceable:Boolean // checks priceablility of the payoff
	
	/*
	 * Price assuming all variables fixed at spot market.
	 */	
	final def price(market:Market):Double = 
	  if (isFixed) price(getFixings)
	  else if (isPriceable) priceImpl(market) 
	  else Double.NaN
	
	def priceImpl(market:Market):Double = price(market.getFixings(variables))
	
	/*
	 * Price given variable fixings, where set of necessary variables are provided by variables function.
	 */	
	final def price(fixings:Map[String, Double]):Double = 
	  if (isFixed) priceImpl(getFixings)
	  else if (isPriceable) priceImpl(fixings) 
	  else Double.NaN
	
	def priceImpl(fixings:Map[String, Double]):Double
	
	/*
	 * Returns price if there's only one variable. Returns NaN in case of >1 variables.
	 */	
	final def price(fixing:Double):Double = 
	  if (isFixed) price(getFixings)
	  else if (isPriceable) priceImpl(fixing) 
	  else Double.NaN
	
	def priceImpl(fixing:Double):Double 
	
	/*
	 * Returns price if there's no variable. Returns NaN in case of >0 variables.
	 */	
	final def price:Double = 
	  if (isFixed) price(getFixings)
	  else if (isPriceable) priceImpl 
	  else Double.NaN
	
	def priceImpl:Double
	
	/*
	 * Price in case of multiple event dates and multiple variables.
	 * Only refers to the last variable by default but can be overridden.
	 */	
	final def price(fixings:List[Map[String, Double]]):Double = 
	  if (isFixed) price(getFixings)
	  else if (isPriceable) priceImpl(fixings) 
	  else Double.NaN
	
	def priceImpl(fixings:List[Map[String, Double]]):Double = if (fixings.isEmpty) price else price(fixings.last)
	
	
	
	/*
	 * Price in case of multiple event dates with only one variable.
	 * Only refers to the last variable by default but can be overridden.
	 */	
	final def price[T:ClassTag](fixings:List[Double]):Double = 
	  if (isFixed) price(getFixings)
	  else if (isPriceable) priceImpl[T](fixings) 
	  else Double.NaN
	
	def priceImpl[T:ClassTag](fixings:List[Double]):Double = if (fixings.isEmpty) price else price(fixings.last)

	/*
	 * Event dates, usually used in case of >1 fixing dates.
	 */	
	def eventDates(d:CalculationPeriod):List[Date] = List(d.eventDate)
	
	/*
	 * Returns FixedPayoff if all variables fixing are available on given date. 
	 * Returns this object if any variables are missing.
	 */	
	def assignFixings(eventDate:Date):Unit = 
	  if (variables.size == 0) {}
	  else {
	    val fixings:Map[String, Double] = variables.map(v => DB.getPriceOn(v, eventDate).collect{case (_, f) => (v, f)}).flatMap(x => x) (collection.breakOut)
	    assignFixings(fixings)
	  }
	
	/*
	 * Returns FixedPayoff if all variables fixing are available on given market. 
	 * Returns this object if any variables are missing.
	 */	
	def assignFixings(market:Market):Unit = assignFixings(market.getFixings(variables))
	
	def missingInputs:Map[String, Double => Payoff] = Map.empty
	
	def description:String
	
	def inputString:String
	
	def jsonString:String
	
	def toString:String
}
 
object Payoff {
  
	def apply(formula:String):Option[Payoff] =
	  if (formula == null || formula.trim.isEmpty) Some(NullPayoff(""))
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
	
	def simpleCashFlow(amount:Double) = FixedPayoff(amount)
}

