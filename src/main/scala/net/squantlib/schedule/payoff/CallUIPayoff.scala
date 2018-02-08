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
 * {"type":"callui","variable":[String],"trigger":Double, "strike":[Double], "add":Double, "mult": Double, "min":Double, "max":Double, "baseAmount":Double, "basket":String, "description":""}
 */
case class CallUIPayoff(
    callVariables:List[String], 
    trigger:Double, 
    strike:List[Double], 
    mult: Double,
    added: Double,
    maxPayoff : Option[Double],
    minPayoff : Option[Double],
    basket: String,
    var fixedPrice: Option[Double],
    override val physical: Boolean,
    baseAmount: Double = 1.0,
    amount:Double = 1.0,
    description:String = null,
    inputString:String = null)(implicit val fixingInfo:FixingInformation) extends Payoff {
  
  val variables = callVariables.toSet
  
  nominal = amount
  
  override val isPriceable:Boolean = !trigger.isNaN && !trigger.isInfinity && strike.forall(s => !s.isNaN && !s.isInfinity)

  override def eventDates(period:CalculationPeriod):List[Date] = {
    if (!isPriceable) List(period.endDate)
    else if (physical) List(period.eventDate, period.paymentDate)
    else List(period.eventDate)
  }
  
  def getPerformance(v:Double):Double = {
    if (!v.isNaN && !v.isInfinity) 1.0 + (v - 1.0) * mult + added
    else Double.NaN
  }

  trait FixingInterpreter[T] {
    def basketPerformance(fixings:T):Option[Double] // Method to be implemented 
    def price(fixings:T, currentFixedPrice:Option[Double]):Double // Method to be implemented

    def getFixedPrice(fixings:T):Option[Double] = {
      basketPerformance(fixings) match {
        case Some(v) if !v.isNaN && !v.isInfinity  => 
          if (v <= trigger) Some(baseAmount)
          else {
            val perf = getPerformance(v)
            (maxPayoff, minPayoff) match {
              case (Some(max), _) if perf >= max => Some(max)
              case (_, Some(min)) if perf <= min => Some(min)
              case _ => None
            }
          }
        case _ => None
      }
    }
    
    def price(fixings:T):Double = {
      if (physical) {
        if (isFixed) price(fixings, fixedPrice)
        else Double.NaN
      }
      else price(fixings, getFixedPrice(fixings))
    }
    
    def price(fixings:List[T]):Double = {
      fixings.lastOption match {
        case Some(lastFixing) => 
          if (physical) {
            val fixingSize = fixings.length
            if (isFixed) price(lastFixing, fixedPrice)
            else if (fixingSize >= 2) price(lastFixing, getFixedPrice(fixings(fixings.length - 2)))
            else Double.NaN
          }
          else price(lastFixing, getFixedPrice(lastFixing))
        case None => Double.NaN
      }
    }
  }

  implicit object MapInterpreter extends FixingInterpreter[Map[String, Double]] {

    override def basketPerformance(fixings:Map[String, Double]):Option[Double] = {
      if (variables.toSet subsetOf fixings.keySet) {
        val fixValues = (0 to callVariables.size - 1).toList.map(i => fixings(callVariables(i)))
        if (fixValues.forall(v => !v.isNaN && !v.isInfinity)) {
          basket match {
            case "average" => Some((fixValues, strike).zipped.map((v, k) => v/k).sum / fixValues.size.toDouble)
            case "max" => Some((fixValues, strike).zipped.map((v, k) => v/k).max)
            case _ => Some((fixValues, strike).zipped.map((v, k) => v/k).min)
          }
        } else None
      } else None
    }

    override def price(fixings:Map[String, Double], currentFixedPrice:Option[Double]):Double = {
      currentFixedPrice match {
        case Some(f) => f
        case None => basketPerformance(fixings).collect{case v => getPerformance(v)}.getOrElse(Double.NaN)
      }
    }
  }
  
  implicit object DoubleInterpreter extends FixingInterpreter[Double] {
    
    override def basketPerformance(fixing:Double):Option[Double] = Some(fixing / strike.head)

    override def price(fixing:Double, currentFixedPrice:Option[Double]):Double = {
      currentFixedPrice match {
        case Some(f) => f
        case None => 
          if (fixing.isNaN || fixing.isInfinity || variables.size != 1 || !isPriceable) Double.NaN
          else basketPerformance(fixing).collect{case v => getPerformance(v)}.getOrElse(Double.NaN)
      }
    }
  }

  def priceSingle[A:FixingInterpreter](fixings:A):Double = implicitly[FixingInterpreter[A]] price fixings
  
  def priceList[A:FixingInterpreter](fixings:List[A]):Double = implicitly[FixingInterpreter[A]] price fixings
  
  override def priceImpl(fixings:List[Map[String, Double]], pastPayments:List[Double]):Double = priceList(fixings)

  override def priceImpl(fixings:Map[String, Double], pastPayments:List[Double]):Double = priceSingle(fixings)
  
  override def priceImpl[T:ClassTag](fixings:List[Double], pastPayments:List[Double]):Double = priceList(fixings)
  
  override def priceImpl(fixing:Double, pastPayments:List[Double]):Double = priceSingle(fixing)
  
  override def priceImpl = Double.NaN

  override def assignFixings(f:Map[String, Double]):Unit = {
    super.assignFixings(f)
    checkKnockIn
  }

  override def clearFixings = {
    super.clearFixings
    fixedPrice = None
  }
  
  def checkKnockIn:Unit = {
    fixedPrice = implicitly[FixingInterpreter[Map[String, Double]]] getFixedPrice(getFixings)
  }
    
//  override def priceImpl(fixings:Map[String, Double]) = 
//    basketPerformance(fixings) match {
//      case Some(v) if !v.isNaN && !v.isInfinity  => 
//        if (v <= trigger) baseAmount
//        else {
//          val perf = 1.0 + (v - 1.0) * mult + added
//          (maxPayoff, minPayoff) match {
//            case (Some(max), Some(min)) => math.max(min, math.min(max, perf))
//            case (Some(max), None) => math.min(max, perf)
//            case (None, Some(min)) => math.max(min, perf)
//            case _ => perf
//          }
//        }
//      case _ => Double.NaN
//    }
//    
//  override def priceImpl(fixing:Double) =
//    if (variables.size != 1 || fixing.isNaN || fixing.isInfinity) Double.NaN
//    else {
//      val ulperf = fixing / strike.head
//      if (ulperf <= trigger) baseAmount
//      else {
//        val perf = 1.0 + (ulperf - 1.0) * mult + added
//        (maxPayoff, minPayoff) match {
//          case (Some(max), Some(min)) => math.max(min, math.min(max, perf))
//          case (Some(max), None) => math.min(max, perf)
//          case (None, Some(min)) => math.max(min, perf)
//          case _ => perf
//        }
//      }
//    }
   
  override def toString =
    baseAmount.asPercent + " [" + trigger.asPercent + "] " + nominal.asPercent + " + " + mult.asPercent + " x " + basket + "([" + variables.mkString(",") + "] / [" + strike.mkString(",") + "]) + " + added.asPercent
  
  override def jsonMapImpl = Map(
    "type" -> "callui", 
    "variable" -> callVariables.toArray, 
    "strike" -> strike.toArray, 
    "trigger" -> trigger, 
    "added" -> added,
    "amount" -> amount,
    "mult" -> mult,
    "max" -> maxPayoff,
    "min" -> minPayoff,
    "basket" -> basket,
    "description" -> description)

  
}

object CallUIPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):CallUIPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)

    CallUIPayoff(
      callVariables = formula.parseJsonStringList("variable").map(_.orNull), 
      trigger = fixed.parseJsonDouble("trigger").getOrElse(Double.NaN), 
      strike = fixed.parseJsonDoubleList("strike").map(_.getOrElse(Double.NaN)), 
      mult = fixed.parseJsonDouble("mult").getOrElse(1.0),
      added = fixed.parseJsonDouble("add").getOrElse(0.0),
      maxPayoff = fixed.parseJsonDouble("max"),
      minPayoff = fixed.parseJsonDouble("min"),
      basket = formula.parseJsonString("basket").getOrElse("worst"),
      fixedPrice = None,
      physical = formula.parseJsonString("physical").getOrElse("0") == "1",
      baseAmount = fixed.parseJsonDouble("baseAmount").getOrElse(1.0),
      amount = fixed.parseJsonDouble("amount").getOrElse(1.0),
      description = formula.parseJsonString("description").orNull,
      inputString = inputString
    )
    
  }
  
}

