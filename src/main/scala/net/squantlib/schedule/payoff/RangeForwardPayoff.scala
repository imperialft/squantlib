package net.squantlib.schedule.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.map.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import java.util.{Map => JavaMap}
import net.squantlib.util.FixingInformation

/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 *  {type:"rangeforward", variable:String, triggerlow:Double, triggerhigh:Double, strike:Double, description:String}, 
 * No strike is considered as no low boundary
 */
case class RangeForwardPayoff(
    variable:String, 
    triggerLow:Option[Double], 
    triggerHigh:Option[Double], 
    strike:Double, 
    var fixedPrice: Option[Double],
    forwardInRange:Boolean = true,
    amount:Double = 1.0, 
    description:String = null,
    inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {
  
  val variables = Set(variable)
  nominal = amount
  
  override val isPriceable:Boolean = 
    (triggerHigh match {
      case Some(v) => !v.isNaN && !v.isInfinity
      case _ => true
    }) && 
    (triggerLow match {
      case Some(v) => !v.isNaN && !v.isInfinity
      case _ => true
    }) && 
    !strike.isNaN && !strike.isInfinity
  
//  def getFixings(fixings:Map[String, Double]):Option[List[Double]] = 
//    if (variables.toSet subsetOf fixings.keySet) 
//      Some((0 to putVariables.size - 1).toList.map(i => fixings(putVariables(i))))
//    else None
      
  override def priceImpl(fixings:Map[String, Double]) = 
    fixings.get(variable) match {
      case Some(v) if !v.isNaN && !v.isInfinity => priceImpl(v)
      case None => Double.NaN
    }
  
  private def satisfyRange(fixing:Double):Boolean = {
    val r = (triggerLow, triggerHigh) match {
      case (Some(l), Some(h)) => fixing >= l && fixing <= h
      case (None, Some(h)) => fixing <= h
      case (Some(l), None) => fixing >= l
      case (None, None) => true
    }
    if (forwardInRange) r else !r
  }
  
  override def priceImpl(fixing:Double):Double =
    if (satisfyRange(fixing)) fixing / strike
    else 1.0

  override def clearFixings = {
    super.clearFixings
    fixedPrice = None
  }
    
   
  override def toString =
    nominal.asPercent + " [" + triggerHigh.asDouble + ", " + triggerLow.asDouble + "] " + nominal.asPercent + " x " + variable + " / " + strike.asDouble + ""
  
  override def priceImpl = Double.NaN
  
  override def jsonMapImpl = Map(
    "type" -> "rangeforward", 
    "variable" -> variable, 
    "triggerlow" -> triggerLow, 
    "triggerhigh" -> triggerHigh, 
    "strike" -> strike, 
    "description" -> description)
  
}

object RangeForwardPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):RangeForwardPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)
    
    val variable:String = formula.parseJsonString("variable").getOrElse(null)
    val triggerHigh:Option[Double] = fixed.parseJsonDouble("triggerhigh")
    val triggerLow:Option[Double] = fixed.parseJsonDouble("triggerlow")
    val strike:Double = fixed.parseJsonDouble("strike").getOrElse(Double.NaN)
    val amount:Double = fixed.parseJsonDouble("amount").getOrElse(1.0)
    val forwardInRange:Boolean = formula.parseJsonString("range_type").orNull == "in"
    val description:String = formula.parseJsonString("description").orNull
    RangeForwardPayoff(variable, triggerLow, triggerHigh, strike, None, forwardInRange, amount, description, inputString)
  }
  
}

