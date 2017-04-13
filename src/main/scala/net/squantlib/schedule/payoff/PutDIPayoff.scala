package net.squantlib.schedule.payoff

import scala.collection.JavaConversions._
import org.codehaus.jackson.map.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import java.util.{Map => JavaMap}
import net.squantlib.util.FixingInformation
import net.squantlib.util.Date
import net.squantlib.schedule.CalculationPeriod
import scala.reflect.ClassTag

/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 *  {type:"putdi", variable:[String], trigger:[Double], strike:[Double], description:String}, 
 * No strike is considered as no low boundary
 */
case class PutDIPayoff(
    putVariables:List[String], 
    trigger:List[Double], 
    strike:List[Double],
    var knockedIn:Boolean,
    override val physical:Boolean,
    amount:Double = 1.0,
    description:String = null,
    inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {
  
  val variables = putVariables.toSet
  
  nominal = amount
  
  val strikeMap:Map[String, Double] = (putVariables zip strike) (collection.breakOut)
   
  val triggerMap:Map[String, Double] = (putVariables zip trigger) (collection.breakOut)
  
  override val isPriceable:Boolean = !trigger.exists(v => v.isNaN || v.isInfinity) && !strike.exists(v => v.isNaN || v.isInfinity)

  override def eventDates(period:CalculationPeriod):List[Date] = {
    if (!isPriceable) List(period.endDate)
    else if (physical) List(period.eventDate, period.paymentDate)
    else List(period.eventDate)
  }

  trait FixingInterpreter[T] {
    def isKnockIn(fixings:T):Boolean // Method to be implemented
    def price(fixings:T, isKnockedIn:Boolean):Double // Method to be implemented
    
    def price(fixings:T):Double = {
      if (physical) {
        if (isFixed) price(fixings, knockedIn)
        else Double.NaN
      }
      else price(fixings, isKnockIn(fixings))
    }
    
    def price(fixings:List[T]):Double = {
      fixings.lastOption match {
        case Some(lastFixing) => 
          if (physical) {
            val fixingSize = fixings.length
            if (isFixed) price(lastFixing, knockedIn)
            else if (fixingSize >= 2) price(lastFixing, isKnockIn(fixings(fixings.length - 2)))
            else Double.NaN
          }
          else price(lastFixing, isKnockIn(lastFixing))
        case None => Double.NaN
      }
    }
  }
  
  implicit object MapInterpreter extends FixingInterpreter[Map[String, Double]] {
    
    override def isKnockIn(fixings:Map[String, Double]):Boolean = {
      variables.exists(p => fixings.get(p) match { 
        case Some(v) if triggerMap.contains(p) => v <= triggerMap(p) 
        case None => false
      })
    }

    override def price(fixings:Map[String, Double], isKnockedIn:Boolean):Double = {
      if (!isKnockedIn) 1.0
      else if ((variables subsetOf fixings.keySet) && variables.forall(v => !fixings(v).isNaN && !fixings(v).isInfinity) && isPriceable) variables.map(v => fixings(v) / strikeMap(v)).min
      else Double.NaN
    }
  }
  
  implicit object DoubleInterpreter extends FixingInterpreter[Double] {

    override def isKnockIn(fixing:Double):Boolean = fixing <= trigger.head

    override def price(fixing:Double, isKnockedIn:Boolean):Double = {
      if (!isKnockedIn) 1.0
      else if (fixing.isNaN || fixing.isInfinity || variables.size != 1 || !isPriceable) Double.NaN
      else fixing / strike.head
    }
  }

  def priceSingle[A:FixingInterpreter](fixings:A):Double = implicitly[FixingInterpreter[A]] price fixings
  
  def priceList[A:FixingInterpreter](fixings:List[A]):Double = implicitly[FixingInterpreter[A]] price fixings
  
  override def priceImpl(fixings:List[Map[String, Double]]):Double = priceList(fixings)

  override def priceImpl(fixings:Map[String, Double]):Double = priceSingle(fixings)
  
  override def priceImpl[T:ClassTag](fixings:List[Double]):Double = priceList(fixings)
  
  override def priceImpl(fixing:Double):Double = priceSingle(fixing)
  
  override def priceImpl = Double.NaN

  override def clearFixings = {
    super.clearFixings
    knockedIn = false
  }

  override def assignFixings(f:Map[String, Double]):Unit = {
    super.assignFixings(f)
    checkKnockIn
  }
    
  def checkKnockIn:Unit = {
    knockedIn = implicitly[FixingInterpreter[Map[String, Double]]] isKnockIn(getFixings)
  }
  
  
//  def getFixings(fixings:Map[String, Double]):Option[List[Double]] = {
//    if (variables.toSet subsetOf fixings.keySet) 
//      Some((0 to putVariables.size - 1).toList.map(i => fixings(putVariables(i))))
//    else None
//  }
//      
//  override def priceImpl(fixings:Map[String, Double]) = {
//    getFixings(fixings) match {
//      case Some(fixValues) if fixValues.forall(v => !v.isNaN && !v.isInfinity) => 
//        if (fixValues.corresponds(trigger) {_ >= _}) 1.0
//        else math.min(1.00, (fixValues, strike).zipped.map((v, k) => v/k).min)
//      case None => Double.NaN
//    }
//  }
//    
//  override def priceImpl(fixing:Double) = {
//    if (variables.size != 1 || fixing.isNaN || fixing.isInfinity) Double.NaN
//    else if (fixing >= trigger.head) 1.0
//    else math.min(1.00, fixing / strike.head)
//  }
  
  override def toString =
    nominal.asPercent + " [" + trigger.mkString(",") + "] " + nominal.asPercent + " x Min([" + variables.mkString(",") + "] / [" + strike.mkString(",") + "])"
  
//  override def priceImpl = Double.NaN
  
  override def jsonMapImpl = Map(
    "type" -> "putdi", 
    "variable" -> putVariables.toArray, 
    "trigger" -> trigger.toArray, 
    "strike" -> strike.toArray, 
    "description" -> description)

  
}

object PutDIPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):PutDIPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)
    val variable:List[String] = formula.parseJsonStringList("variable").map(_.orNull)  
    val trigger:List[Double] = fixed.parseJsonDoubleList("trigger").map(_.getOrElse(Double.NaN))
    val strike:List[Double] = fixed.parseJsonDoubleList("strike").map(_.getOrElse(Double.NaN))
    val amount:Double = fixed.parseJsonDouble("amount").getOrElse(1.0)
    val description:String = formula.parseJsonString("description").orNull
    val physical:Boolean = formula.parseJsonString("physical").getOrElse("0") == "1"
    PutDIPayoff(variable, trigger, strike, false, physical, amount, description, inputString)
  }
  
}

