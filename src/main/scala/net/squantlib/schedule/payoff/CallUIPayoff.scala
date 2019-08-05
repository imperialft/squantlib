package net.squantlib.schedule.payoff

import scala.collection.JavaConversions._
import com.fasterxml.jackson.databind.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import java.util.{Map => JavaMap}

import net.squantlib.util.{Date, FixingInformation, UnderlyingFixing}
import net.squantlib.schedule.CalculationPeriod

import scala.reflect.ClassTag


/**
 * Interprets JSON formula specification for sum of linear formulas with discrete range.
 * JSON format:
 * {"type":"callui","variable":[String],"trigger":Double, "strike":[Double], "add":Double, "mult": Double, "min":Double, "max":Double, "baseAmount":Double, "basket":String, "description":""}
 */
case class CallUIPayoff(
  strike:UnderlyingFixing,
  trigger:Double,
  mult: Double,
  added: Double,
  override val minPayoff : Double,
  override val maxPayoff : Option[Double],
  basket: String,
  var fixedPrice: Option[Double],
  override val physical: Boolean,
  baseAmount: Double = 1.0,
  amount:Double = 1.0,
  description:String = null,
  inputString:String = null
)(implicit val fixingInfo:FixingInformation) extends Payoff {

  val variables = strike.keySet

  nominal = amount
  
  override val isPriceable:Boolean = !strike.isEmpty && strike.getDecimal.values.forall(_.isDefined) && strike.getDouble.values.forall(_ > 0.000000001)

  override def eventDates(period:CalculationPeriod):List[Date] = {
    if (!isPriceable) List(period.endDate)
    else if (physical) List(period.eventDate, period.paymentDate)
    else List(period.eventDate)
  }
  
  def getPerformance(v:Double):Double = {
    if (!v.isNaN && !v.isInfinity) {
      //1.0 + (v - 1.0) * mult + added
      (v - 1.0) * mult + added
    }
    else Double.NaN
  }

  def getFixedPrice(fixings:UnderlyingFixing, priceResult:PriceResult):Option[Double] = {
    basketPerformance(fixings, priceResult) match {
      case Some(v) if !v.isNaN && !v.isInfinity  =>
        if (v <= trigger) Some(baseAmount)
        else {
          Some(withMinMax(getPerformance(v)))
        }
      case _ => None
    }
  }

  def priceList(fixings:UnderlyingFixing, priceResult:PriceResult):Double = {
    if (physical) {
      if (isFixed) priceList(fixings, fixedPrice, priceResult)
      else Double.NaN
    }
    else priceList(fixings, getFixedPrice(fixings, priceResult), priceResult)
  }

  def priceList(fixings:List[UnderlyingFixing], priceResult:PriceResult):Double = {
    fixings.lastOption match {
      case Some(lastFixing) =>
        if (physical) {
          val fixingSize = fixings.length
          if (isFixed) priceList(lastFixing, fixedPrice, priceResult)
          else if (fixingSize >= 2) priceList(lastFixing, getFixedPrice(fixings(fixings.length - 2), priceResult), priceResult)
          else Double.NaN
        }
        else priceList(lastFixing, getFixedPrice(lastFixing, priceResult), priceResult)
      case None => Double.NaN
    }
  }

  def basketPerformance(fixings:UnderlyingFixing, priceResult:PriceResult):Option[Double] = {
    if (fixings.isValidFor(variables)) {
      val perfs:Map[String, Double] = strike.getDouble.map{case (ul, v) => (ul, fixings.getDouble(ul) / v.toDouble)}

      if (perfs.forall{case (ul, v) => !v.isNaN && !v.isInfinity}) {

        basket match {
          case "average" => Some(perfs.values.sum / perfs.size.toDouble)

          case "max" =>
            if (physical && priceResult != null) {
              perfs.maxBy{case (ul, pf) => pf} match {
                case (ul, pf) => priceResult.setAssetInfo(ul, 1.0 / strike.getDouble(ul))
              }
            }

            Some(perfs.values.max)

          case _ =>

            if (physical && priceResult != null) {
              perfs.minBy{case (ul, pf) => pf} match {
                case (ul, pf) => priceResult.setAssetInfo(ul, 1.0 / strike.getDouble(ul))
              }
            }

            Some(perfs.values.min)

        }
      } else None
    } else None
  }

  def priceList(fixings:UnderlyingFixing, currentFixedPrice:Option[Double], priceResult:PriceResult):Double = {
    currentFixedPrice match {
      case Some(f) => f
      case None => basketPerformance(fixings, priceResult).collect{case v => getPerformance(v)}.getOrElse(Double.NaN)
    }
  }
//  }

  //def priceList[A:FixingInterpreter](fixings:List[A], priceResult:PriceResult):Double = implicitly[FixingInterpreter[A]] price(fixings, priceResult)
  
  override def priceImpl(fixings:List[UnderlyingFixing], pastPayments:List[Double], priceResult:PriceResult):Double = priceList(fixings, priceResult)

  override def priceImpl(priceResult:PriceResult) = {
    Double.NaN
  }

  override def assignFixings(f:UnderlyingFixing):Unit = {
    super.assignFixings(f)
    checkKnockIn
  }

  override def clearFixings = {
    super.clearFixings
    fixedPrice = None
  }
  
  def checkKnockIn:Unit = {
    fixedPrice = getFixedPrice(getFixings, null) //implicitly[FixingInterpreter[Map[String, Double]]] getFixedPrice(getFixings, null)
  }
    
  override def toString =
    baseAmount.asPercent + " [" + trigger.asPercent + "] " + nominal.asPercent + " + " + mult.asPercent + " x " + basket + "([" + variables.mkString(",") + "] / [" + strike.getDecimalValue.mkString(",") + "]) + " + added.asPercent
  
  override def jsonMapImpl = Map(
    "type" -> "callui", 
    "strike" -> strike.getDouble.map{case (ul, v) => (ul, v)},
    "trigger" -> trigger,
    "add" -> added,
    "amount" -> amount,
    "mult" -> mult,
    "max" -> maxPayoff,
    "min" -> minPayoff,
    "basket" -> basket,
    "description" -> description
  )

  override def fixedConditions:Map[String, Any] = {
    Map(
      "strike" -> strike.getDouble.map{case (ul, v) => (ul, v)},
    )
  }


}

object CallUIPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):CallUIPayoff = {
    val formula = Payoff.updateReplacements(inputString)
    val fixed = fixingInfo.update(formula)
    val fixedNode = fixed.jsonNode
    val callVariables = formula.parseJsonStringList("variable").map(_.orNull)
//    val strikeList:List[Double] = fixed.parseJsonDoubleList("strike").map(_.getOrElse(Double.NaN))
//    val strikes = (callVariables, strikeList).zipped.toMap.getOptionalDecimal
    val strikes:UnderlyingFixing = UnderlyingFixing(fixedNode.collect{case n => Payoff.nodeToComputedMap(n, "strike", callVariables)}.getOrElse(Map.empty))


    CallUIPayoff(
      strike = strikes,
      trigger = fixed.parseJsonDouble("trigger").getOrElse(Double.NaN),
      mult = fixed.parseJsonDouble("mult").getOrElse(1.0),
      added = fixed.parseJsonDouble("add").getOrElse(0.0),
      minPayoff = fixed.parseJsonDouble("min").getOrElse(0.0),
      maxPayoff = fixed.parseJsonDouble("max"),
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

