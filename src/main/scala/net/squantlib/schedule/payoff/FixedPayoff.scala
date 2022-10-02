package net.squantlib.schedule.payoff

import scala.collection.JavaConversions._
import com.fasterxml.jackson.databind.ObjectMapper
import net.squantlib.util.DisplayUtils._
import net.squantlib.util.JsonUtils._
import net.squantlib.util.{FixingInformation, UnderlyingFixing}
import net.squantlib.schedule.KnockInCondition

/**
 * Interprets JSON formula specification for a fixed leg.
 * JSON format: {type:"fixed", description:XXX, payoff:double}
 * Natual format: 0.035 or "3.5%"
 */
case class FixedPayoff(
  payoff:BigDecimal,
  resetKnockInCondition: KnockInCondition,
  resetPayoff: Option[Payoff],
  var isReset:Boolean,
  description:String = null,
  inputString:String = null
)(implicit val fixingInfo:FixingInformation) extends Payoff {

  def adjustedPayoff:Option[Payoff] = {
    if (isReset) resetPayoff
    else None
  }

  override val variables: Set[String] = adjustedPayoff match {
    case Some(p) => p.variables
    case _ => Set.empty
  }

  override val isPriceable:Boolean = adjustedPayoff match {
    case Some(p) => p.isPriceable
    case _ => true // !payoff.isNaN && !payoff.isInfinity
  }
  
  override val isFixed = true
   
  // override def priceImpl(
  //   fixings:List[UnderlyingFixing],
  //   pastPayments:List[Double],
  //   priceResult:PriceResult
  // ):Double = payoff.toDouble

  override def priceImpl(
    fixings:List[UnderlyingFixing],
    pastPayments:List[Double],
    priceResult:PriceResult
  ):Double = adjustedPayoff match {
    case Some(p) => p.priceImpl(fixings, pastPayments, priceResult)
    case _ => payoff.toDouble
  }

  override def priceImpl(priceResult:PriceResult):Double = adjustedPayoff match {
    case Some(p) => p.priceImpl(priceResult)
    case _ => payoff.toDouble
  }
  
  override def toString = adjustedPayoff match {
    case Some(p) => p.toString
    case _  => payoff.asPercent
  }
  
  override def jsonMapImpl = adjustedPayoff match {
    case Some(p) => p.jsonMapImpl
    case _ => Map(
      "type" -> "fixed", 
      "description" -> description, 
      "payoff" -> payoff
    )
  }

  override def fixedConditions:Map[String, Any] = adjustedPayoff match {
    case Some(p) => p.fixedConditions.updated("reset", true)
    case _ => Map("reset" -> false)
  }

}


object FixedPayoff {
  
  def apply(inputString:String)(implicit fixingInfo:FixingInformation):FixedPayoff = {
    val formula = Payoff.updateReplacements(inputString)

    val resetKnockInCondition:KnockInCondition = formula.jsonNode.collect{case node => Payoff.initializeCouponReset(node)(fixingInfo.getStrikeFixingInformation)}.getOrElse(KnockInCondition.empty)

    val resetPayoff:Option[Payoff] = {
      if (resetKnockInCondition.isEmpty) None
      else inputString.jsonNode("reset_payoff") match {
        case Some(node) => Payoff(node.toJsonString)
        case _ => None
      }
    }

    val isReset:Boolean = {
      if (resetKnockInCondition.isEmpty) false
      else resetKnockInCondition.isKnockedIn
    }
    
    fixingInfo.update(formula).parseDouble match {
      case Some(v) => FixedPayoff(
        payoff = v, 
        resetKnockInCondition = resetKnockInCondition,
        resetPayoff = resetPayoff,
        isReset = isReset,
        description = null, 
        inputString = inputString
      )

      case None => FixedPayoff(
        payoff = fixingInfo.update(formula).parseJsonDouble("payoff").getOrElse(Double.NaN), 
        resetKnockInCondition = resetKnockInCondition,
        resetPayoff = resetPayoff,
        isReset = isReset,
        description = formula.parseJsonString("description").orNull, 
        inputString = inputString
      )
    }
  }
	
  def apply(
    payoff:Double
 )(implicit fixingInfo:FixingInformation):FixedPayoff = new FixedPayoff(
    payoff = payoff,
    resetKnockInCondition = KnockInCondition.empty,
    resetPayoff = None,
    isReset = false,
    description = null,
    inputString = null
  )

  def apply(
    payoff:BigDecimal
 )(implicit fixingInfo:FixingInformation):FixedPayoff = new FixedPayoff(
    payoff = payoff,
    resetKnockInCondition = KnockInCondition.empty,
    resetPayoff = None,
    isReset = false,
    description = null,
    inputString = null
  )

  def apply(
    payoff: Double,
    description: String,
    inputString: String
  )(implicit fixingInfo:FixingInformation):FixedPayoff = FixedPayoff(
    payoff = payoff.getRoundedDecimal.getOrElse(BigDecimal(0.0)),
    resetKnockInCondition = KnockInCondition.empty,
    resetPayoff = None,
    isReset = false,
    description = description,
    inputString = inputString
  )

  def apply(
    payoff: Double,
    resetKnockInCondition: KnockInCondition,
    resetPayoff: Option[Payoff],
    isReset:Boolean,
    description: String,
    inputString: String
  )(implicit fixingInfo:FixingInformation):FixedPayoff = FixedPayoff(
    payoff = payoff.getRoundedDecimal.getOrElse(BigDecimal(0.0)),
    resetKnockInCondition = resetKnockInCondition,
    resetPayoff = resetPayoff,
    isReset = isReset,
    description = description,
    inputString = inputString
  )
}