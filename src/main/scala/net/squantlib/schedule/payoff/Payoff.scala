package net.squantlib.schedule.payoff

import java.util.{Map => JavaMap}
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.util.FormulaParser
import net.squantlib.database.DB
import net.squantlib.util.FixingInformation
import net.squantlib.util.Date
import net.squantlib.model.market.Market
import scala.Option.option2Iterable
import net.squantlib.schedule.CalculationPeriod
import net.squantlib.schedule.FixingLeg
import scala.reflect.ClassTag
import scala.collection.JavaConversions._
import scala.collection.JavaConversions._
import org.codehaus.jackson.map.ObjectMapper

trait Payoff extends FixingLeg {
  
  /*
   * List of reference variables. 
   * Price fixings should be provided by map variable -> fixing value.
   */  
  override val variables:Set[String]
  
  val isPriceable:Boolean // checks priceablility of the payoff
  
  val fixingInfo:FixingInformation
  
  var isAbsolute:Boolean = false
  
  var nominal:Double = 1.0
  
//  var targetRedemption:Option[Double] = None
//  
//  var targetCapped:Boolean = false
  
  val keywords:Set[String] = jsonMapImpl.keySet
  
  def jsonMap:Map[String, Any] = inputString.jsonNode match {
    case Some(node) =>
      val fieldnames = node.getFieldNames.filter(n => !keywords.contains(n)).toSet
      fieldnames.map(n => (n -> node.get(n))).toMap ++ jsonMapImpl
    case _ => jsonMapImpl
  }
  
  def jsonString:String = {
    val infoMap:JavaMap[String, Any] = jsonMap
    (new ObjectMapper).writeValueAsString(infoMap)
  }

  def jsonMapImpl:Map[String, Any]  
  
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
  
  def toString:String
}
 
object Payoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):Option[Payoff] = {
    if (inputString == null || inputString.trim.isEmpty) Some(NullPayoff(""))
    else {
      val po = payoffType(inputString) match {
        case "fixed" => FixedPayoff(inputString)
        case "leps1d" => LEPS1dPayoff(inputString)
        case "linear1d" => Linear1dPayoff(inputString)
        case "putdi" => PutDIPayoff(inputString)
        case "putdiamerican" => PutDIAmericanPayoff(inputString)
        case "forward" => ForwardPayoff(inputString)
        case "rangeforward" => RangeForwardPayoff(inputString)
        case "null" => NullPayoff(inputString)
        case "binary" => BinaryPayoff(inputString)
        case "general" => GeneralPayoff(inputString)
        case _ => GeneralPayoff(inputString)
      }
      
      if (isAbsolute(inputString)) {po.isAbsolute = true}
      customNominal(inputString) match {
        case Some(n) => po.nominal = n
        case _ => {}
      }

//      targetRedemption(inputString) match {
//        case Some(n) => 
//          po.targetRedemption = Some(n)
//          po.targetCapped = targetCap(inputString)
//        case _ => {}
//      }
      
      Some(po)
    }
  }
  
  def payoffType(formula:String):String = formula match {
    case f if f.parseDouble.isDefined => "fixed"
    case f if f.startsWith("leps") => "leps1d"
    case f => formula.parseJsonString("type").orNull
    }
  
  def isAbsolute(formula:String):Boolean = formula match {
    case f if f.parseDouble.isDefined => false
    case f => formula.parseJsonString("absolute") == Some("1")
    }
  
  def customNominal(formula:String):Option[Double] = formula match {
    case f if f.parseDouble.isDefined => None
    case f => formula.parseJsonDouble("nominal")
    }
  
//  def targetRedemption(formula:String):Option[Double] = formula match {
//    case f if f.parseDouble.isDefined => None
//    case f => formula.parseJsonDouble("target_redemption")
//    }
//
//  def targetCap(formula:String):Boolean = formula match {
//    case f if f.parseDouble.isDefined => false
//    case f => formula.parseJsonString("target_cap") == Some("1")
//    }
  
  def simpleCashFlow(amount:Double) = FixedPayoff(amount)(FixingInformation.empty)
  
  def updateReplacements(inputString:String)(implicit fixingInfo:FixingInformation):String = {
    if (!inputString.trim.startsWith("{")) {return inputString}
    
    var result = inputString
    inputString.jsonNode match {
      case Some(node) => 
	    val fieldnames = node.getFieldNames.filter(n => n.startsWith("^")).toSet
        fieldnames.foreach(n => {
          val currentvalue = node.get(n).getTextValue()
          val updatedvalue = fixingInfo.update(currentvalue)
          result = result.replace(n, FormulaParser.calculate(updatedvalue).getOrElse(Double.NaN).toString)
        })
	    case None => {}
	  }
    result
  }
    
}

